;;;; Copyright 2011, Google Inc.  All rights reserved.

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;     * Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;;     * Redistributions in binary form must reproduce the above
;;;; copyright notice, this list of conditions and the following disclaimer
;;;; in the documentation and/or other materials provided with the
;;;; distribution.
;;;;     * Neither the name of Google Inc. nor the names of its
;;;; contributors may be used to endorse or promote products derived from
;;;; this software without specific prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Author: Robert Brown <robert.brown@gmail.com>

;;;; Snappy compression of octet vectors.

(in-package #:snappy)

(defconst +maximum-hash-bits+ 14)
(defconst +maximum-hash-table-size+ (expt 2 +maximum-hash-bits+))

(deftype hash-result () `(unsigned-byte ,+maximum-hash-bits+))
(deftype table-size () `(integer 0 ,+maximum-hash-table-size+))

(defconst +literal+ 0)

(defconst +copy-1-byte-offset+ 1)
(defconst +copy-2-byte-offset+ 2)
(defconst +copy-4-byte-offset+ 3)

(defconst +vector-index-bits+ (integer-length +maximum-vector-index+))

(defmacro postincf (variable)
  (check-type variable symbol)
  `(prog1 ,variable (incf ,variable)))

(declaim (ftype (function (octet-vector vector-index hash-result)
                          (values hash-result &optional))
                hash)
         (inline hash))

(defun hash (buffer index mask)
  (declare (type octet-vector buffer)
           (type vector-index index)
           (type hash-result mask))
  (logand (logxor (ash (logxor (ash (aref buffer (+ index 3)) 6)
                               (ash (aref buffer (+ index 2)) 5)
                               (aref buffer (+ index 1)))
                       5)
                  (aref buffer index))
          mask))

(declaim (ftype (function (vector-index) (values vector-index &optional))
                maximum-compressed-length))

(defun maximum-compressed-length (uncompressed-length)
  "Returns the maximum size a vector of length UNCOMPRESSED-LENGTH may take up
after it is compressed."
  (declare (type vector-index uncompressed-length))
  (+ 32 uncompressed-length (floor uncompressed-length 6)))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values vector-index &optional))
                uncompressed-length))

(defun uncompressed-length (buffer index limit)
  "Returns the uncompressed length of the compressed data stored in BUFFER from
position INDEX to LIMIT."
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (let ((length (parse-uint32-carefully buffer index limit)))
    (check-type length vector-index)
    length))

(declaim (ftype (function (octet-vector vector-index octet-vector vector-index vector-index)
                          (values vector-index &optional))
                emit-literal))

(defun emit-literal (input-buffer literal output-buffer out length)
  (declare (type octet-vector input-buffer output-buffer)
           (type vector-index literal out length))
  (let ((n (1- length)))                ; no literal has length zero
    (declare (type vector-index n))
    (if (< n 60)
        (setf (aref output-buffer (postincf out)) (logior +literal+ (ash n 2)))
        (let ((base out)
              (count 0))
          (declare (type (integer 0 4) count))
          (incf out)
          (loop while (plusp n) do
            (setf (aref output-buffer out) (ldb (byte 8 0) n))
            (incf out)
            (setf n (ash n -8))
            (incf count))
          ;; (assert (<= 1 count 4))
          (setf (aref output-buffer base)
                (logior +literal+ (ash (+ count 59) 2)))))
    (replace output-buffer input-buffer
             :start1 out :end1 (+ out length)
             :start2 literal :end2 (+ literal length))
    (the vector-index (+ out length))))

(declaim (ftype (function (octet-vector vector-index vector-index (integer 4 64))
                          (values vector-index &optional))
                emit-copy-less-than-64))

(defun emit-copy-less-than-64 (output-buffer out offset length)
  (declare (type octet-vector output-buffer)
           (type vector-index out offset)
           (type (integer 4 64) length))
  ;; (assert (<= 4 length 64))
  (cond ((and (< length 12) (< offset 2048))
         (let ((length-4 (- length 4)))
           ;; (assert (< length-4 8))
           (setf (aref output-buffer (postincf out))
                 (logior +copy-1-byte-offset+
                         (ash length-4 2)
                         (ash (ldb (byte 3 8) offset) 5)))
           (setf (aref output-buffer (postincf out)) (ldb (byte 8 0) offset))))
        ((< offset 65536)
         (setf (aref output-buffer (postincf out))
               (logior +copy-2-byte-offset+ (ash (1- length) 2)))
         (setf (ub16ref/le output-buffer out) offset)
         (incf out 2))
        (t
         (setf (aref output-buffer (postincf out))
               (logior +copy-4-byte-offset+ (ash (1- length) 2)))
         (setf (ub32ref/le output-buffer out) offset)
         (incf out 4)))
  out)

(declaim (ftype (function (octet-vector vector-index vector-index vector-index)
                          (values vector-index &optional))
                emit-copy))

(defun emit-copy (output-buffer out offset length)
  (declare (type octet-vector output-buffer)
           (type vector-index out offset length))
  ;; Emit 64 byte copies, but make sure to keep at least four bytes
  ;; reserved.
  (loop while (>= length 68) do
    (setf out (emit-copy-less-than-64 output-buffer out offset 64))
    (decf length 64))
  ;; Emit an extra 60 byte copy, if have too much data to fit in one copy.
  (when (> length 64)
    (setf out (emit-copy-less-than-64 output-buffer out offset 60))
    (decf length 60))
  ;; Emit remainder.
  (emit-copy-less-than-64 output-buffer out offset length))

(declaim (ftype (function (octet-vector vector-index vector-index
                           octet-vector vector-index vector-index)
                          (values vector-index &optional))
                raw-compress))

(defun raw-compress (input-buffer in in-limit output-buffer out out-limit)
  (declare (type octet-vector input-buffer output-buffer)
           (type vector-index in in-limit out out-limit))
  ;; (assert (>= +maximum-hash-table-size+ 256))
  (let* ((in-length (- in-limit in))
         (table-size (loop for size of-type table-size = 256 then (* 2 size)
                           while (and (< size +maximum-hash-table-size+)
                                      (< size in-length))
                           finally (return size)))
         (mask (1- table-size))
         (table (make-array table-size
                            :element-type 'vector-index :initial-element +maximum-vector-index+))
         (next-emit in)
         (safe-in-limit (- in-limit 13))
         (initial-out out))
    (setf out (encode-uint32-carefully output-buffer out out-limit in-length))
    (do ()
        ((>= in safe-in-limit))
      (let* ((k (hash input-buffer in mask))
             (candidate (aref table k)))
        (setf (aref table k) in)

        (when (= candidate +maximum-vector-index+)
          ;; No candidate match, so continue.
          (incf in)
          (go continue))

        ;; Check for a match of at least 4 octets.
        (when (or (/= (aref input-buffer candidate)
                      (aref input-buffer in))
                  (/= (aref input-buffer (1+ candidate))
                      (aref input-buffer (1+ in)))
                  (/= (aref input-buffer (+ candidate 2))
                      (aref input-buffer (+ in 2)))
                  (/= (aref input-buffer (+ candidate 3))
                      (aref input-buffer (+ in 3))))
          ;; No match, so continue.
          (incf in)
          (go continue))

        ;; We have found a match of at least 4 characters.
        (let ((base in)
              (match-count 0))
          (declare (type vector-index match-count))
          (incf in 4)
          ;;  Find out how long the match is.
          (if (or (/= (aref input-buffer (+ candidate 4))
                      (aref input-buffer (postincf in)))
                  (/= (aref input-buffer (+ candidate 5))
                      (aref input-buffer (postincf in)))
                  (/= (aref input-buffer (+ candidate 6))
                      (aref input-buffer (postincf in)))
                  (/= (aref input-buffer (+ candidate 7))
                      (aref input-buffer (postincf in)))
                  (/= (aref input-buffer (+ candidate 8))
                      (aref input-buffer (postincf in)))
                  (/= (aref input-buffer (+ candidate 9))
                      (aref input-buffer (postincf in)))
                  (/= (aref input-buffer (+ candidate 10))
                      (aref input-buffer (postincf in))))
              (progn
                ;; Short match
                (decf in)
                (setf match-count (- in base)))
              (progn
                ;; Longer match
                (setf match-count (- in base))
                (loop while (and (< in safe-in-limit)
                                 (= (aref input-buffer in)
                                    (aref input-buffer
                                          (+ candidate match-count))))
                      do (incf in)
                         (incf match-count))))
          (let ((offset (- base candidate)))
            (when (and (= match-count 4) (>= offset 65536))
              ;; Encoding of copy operation takes 5 bytes, so do not bother
              ;; with this match.
              (setf in (1+ base))
              (go continue))
            (when (< next-emit base)
              (setf out (emit-literal input-buffer next-emit
                                      output-buffer out
                                      (- base next-emit))))
            (setf out (emit-copy output-buffer out offset match-count))
            (when (>= match-count 16)
              (loop for i from (- match-count 16) below match-count by 4 do
                (setf (aref table (hash input-buffer (+ base i) mask)) (+ base i))))
            (setf next-emit in))))
     CONTINUE)

    (when (< next-emit in-limit)
      (setf out (emit-literal input-buffer next-emit
                              output-buffer out
                              (- in-limit next-emit))))
    (- out initial-out)))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values octet-vector vector-index &optional))
                compress))

(defun compress (buffer index limit)
  "Compresses the contents of BUFFER, a vector of (UNSIGNED-BYTE 8), from
position INDEX to position LIMIT.  Returns two values, a vector of
type (UNSIGNED-BYTE 8) holding the compressed data and an integer indicating
the number of compressed octets in the vector."
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (let* ((length (- limit index))
         (max-compressed-length (maximum-compressed-length length))
         (compressed (make-octet-vector max-compressed-length))
         (compressed-length (raw-compress buffer index limit
                                          compressed 0 max-compressed-length)))
    (values compressed compressed-length)))

(declaim (ftype (function (octet-vector vector-index vector-index
                           octet-vector vector-index vector-index)
                          (values vector-index &optional))
                raw-uncompress))

(defun raw-uncompress (input-buffer in in-limit output-buffer out out-limit)
  (declare (type octet-vector input-buffer output-buffer)
           (type vector-index in in-limit out out-limit))
  (multiple-value-bind (uncompressed-size in)
      (parse-uint32-carefully input-buffer in in-limit)
    (check-type uncompressed-size vector-index)
    (let ((initial-out out))
      (when (> uncompressed-size (- out-limit out))
        (error "output buffer too small"))
      (flet ((literal-length (octet)
               (let ((count (ldb (byte 6 2) octet)))
                 (1+ (if (< count 60)
                         count
                         (let ((n 0))
                           (declare (type vector-index n))
                           (decf count 59)
                           (when (> (+ in count) in-limit)
                             (error "input buffer exhausted 1"))
                           (loop for i from (1- count) downto 0 do
                             (setf n (ash n 8))
                             (setf (ldb (byte 8 0) n) (aref input-buffer (+ in i))))
                           (incf in count)
                           n)))))
             (copy-parameters (octet opcode)
               (let ((offset (aref input-buffer in))
                     (length 0))
                 (incf in)
                 (ecase opcode
                   ((#.+copy-1-byte-offset+)
                    (when (>= in in-limit)
                      (error "input buffer exhausted 2"))
                    (setf length (+ (ldb (byte 3 2) octet) 4))
                    (setf (ldb (byte 3 8) offset) (ldb (byte 3 5) octet)))
                   ((#.+copy-2-byte-offset+)
                    (when (>= (1+ in) in-limit)
                      (error "input buffer exhausted 3"))
                    (setf length (1+ (ldb (byte 6 2) octet)))
                    (setf (ldb (byte 8 8) offset) (aref input-buffer in))
                    (incf in))
                   ((#.+copy-4-byte-offset+)
                    (when (>= (+ in 3) in-limit)
                      (error "input buffer exhausted 4"))
                    (setf length (1+ (ldb (byte 6 2) octet)))
                    (setf (ldb (byte 8 8) offset) (aref input-buffer in))
                    (incf in)
                    (setf (ldb (byte 8 16) offset) (aref input-buffer in))
                    (incf in)
                    (setf (ldb (byte #.(- +vector-index-bits+ 24) 24)
                               offset)
                          (aref input-buffer in))
                    (incf in)))
                 (values offset length))))
        (loop while (< in in-limit) do
          (let* ((octet (aref input-buffer in))
                 (opcode (ldb (byte 2 0) octet)))
            (incf in)
            (if (= opcode +literal+)
                ;; Copy literal into output-buffer.
                (let ((length (literal-length octet)))
                  (when (> (+ in length) in-limit)
                    (error "input buffer exhausted 5"))
                  (when (> (+ out length) out-limit)
                    (error "output buffer too small"))
                  (replace output-buffer input-buffer
                           :start1 out :end1 (+ out length)
                           :start2 in :end2 (+ in length))
                  (incf in length)
                  (incf out length))
                ;; Copy from an earlier position in output-buffer.
                (multiple-value-bind (offset length)
                    (copy-parameters octet opcode)
                  (let ((copy (- out offset)))
                    (when (< copy initial-out)
                      (error "invalid offset 1"))
                    ;; Source and destination may overlap.
                    (loop repeat length do
                      (setf (aref output-buffer out) (aref output-buffer copy))
                      (incf out)
                      (incf copy))))))))))
  out)

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values octet-vector &optional))
                uncompress))

(defun uncompress (buffer index limit)
  "Uncompresses BUFFER, a vector of (UNSIGNED-BYTE 8), from position INDEX to
LIMIT.  Returns the uncompressed data as a vector of (UNSIGNED-BYTE 8)."
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (let* ((uncompressed-length (uncompressed-length buffer index limit))
         (uncompressed (make-octet-vector uncompressed-length))
         (actual-uncompressed-length
          (raw-uncompress buffer index limit
                          uncompressed 0 uncompressed-length)))
    (declare (type vector-index actual-uncompressed-length))
    (assert (= actual-uncompressed-length uncompressed-length))
    uncompressed))



;; XXXX: Not yet used.  Create a lookup table for speeding decompression.

(defconst +word-mask+
  (make-array 5
              :element-type 'uint32
              :initial-contents '(0 #xff #xffff #xffffff #xffffffff))
  "Mapping from i in range [0,4] to a mask to extract the bottom 8*i bits.")

(deftype decompression-entry () '(unsigned-byte 16))

(defconst +decompression-table+
  (labels ((make-entry (extra length copy-offset)
             (assert (<= (integer-length extra) 3))
             (assert (<= (integer-length copy-offset) 3))
             (assert (<= (integer-length length) 7))
             (logior length (ash copy-offset 8) (ash extra 11))))
    (declare #.*optimize-default*)
    ;; Initialize dst with invalid entries.
    (let* ((illegal-entry #xffff)
           (dst (make-array 256
                            :element-type 'decompression-entry
                            :initial-element illegal-entry))
           (assigned 0))
      ;; Small literal entries.  We store (len-1) in the top 6 bits.
      (loop for length from 1 to 60 do
        (setf (aref dst (logior +literal+ (ash (1- length) 2)))
              (make-entry 0 length 0))
        (incf assigned))
      ;; Large +LITERAL+ entries.  We use 60..63 in the high 6 bits to
      ;; encode the number of bytes of length info that follow the opcode.
      (loop for extra-bytes from 1 to 4 do
        (setf (aref dst (logior +literal+ (ash (+ extra-bytes 59) 2)))
              (make-entry extra-bytes 1 0))
        (incf assigned))
      ;; +COPY-1-BYTE-OFFSET+
      ;; The tag byte in the compressed data stores len-4 in 3 bits, and
      ;; offset/256 in 5 bits.  offset%256 is stored in the next byte.
      ;; This format is used for length in range [4..11] and offset in
      ;; range [0..2047]
      (loop for length from 4 below 12 do
        (loop for offset from 0 below 2048 by 256 do
          (setf (aref dst (logior +copy-1-byte-offset+
                                  (ash (- length 4) 2)
                                  (ash (ash offset -8) 5)))
                (make-entry 1 length (ash offset -8)))
          (incf assigned)))
      ;; +COPY-2-BYTE-OFFSET+
      ;; Tag contains len-1 in top 6 bits, and offset in next two bytes.
      (loop for length from 1 to 64 do
        (setf (aref dst (logior +copy-2-byte-offset+ (ash (1- length) 2)))
              (make-entry 2 length 0))
        (incf assigned))
      ;; +COPY-4-BYTE-OFFSET+
      ;; Tag contents len-1 in top 6 bits, and offset in next four bytes.
      (loop for length from 1 to 64 do
        (setf (aref dst (logior +copy-4-byte-offset+ (ash (1- length) 2)))
              (make-entry 4 length 0))
        (incf assigned))
      ;; Check that each entry was initialized exactly once.
      (assert (= assigned 256))
      (assert (not (find illegal-entry dst)))
      dst))
  "Data stored per entry in lookup table:
    Range   Bits-used       Description
    ------------------------------------
    1..64   0..7            Literal/copy length encoded in opcode byte
    0..7    8..10           Copy offset encoded in opcode byte / 256
    0..4    11..13          Extra bytes after opcode

We use eight bits for the length even though 7 would have sufficed
because of efficiency reasons:
    (1) Extracting a byte is faster than a bit field
    (2) It properly aligns copy offset so we do not need a <<8")
