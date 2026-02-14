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

(defconst +maximum-block-size+ 65536)
(defconst +input-margin+ (- 16 1))
(defconst +minimum-non-literal-block-size+ (+ 1 1 +input-margin+))

(defconst +maximum-snappy-index+ (1- (expt 2 32)) "Largest valid index into Snappy data.")

(deftype snappy-index ()
  "Integer that can be used as a subscript for accessing a Snappy data vector."
 `(integer 0 #.+maximum-snappy-index+))

(defconst +minimum-hash-bits+ 8)
(defconst +minimum-hash-table-size+ (expt 2 +minimum-hash-bits+))

(defconst +maximum-hash-bits+ 14)
(defconst +maximum-hash-table-size+ (expt 2 +maximum-hash-bits+))

(deftype table-size () `(integer 0 ,+maximum-hash-table-size+))

(defconst +minimum-hash-shift+ (- 32 +maximum-hash-bits+))
(defconst +maximum-hash-shift+ (- 32 +minimum-hash-bits+))

(deftype hash-shift () `(integer ,+minimum-hash-shift+ ,+maximum-hash-shift+))
(deftype hash-result () `(unsigned-byte ,+maximum-hash-bits+))

(defconst +literal+ 0)

(defconst +copy-1-byte-offset+ 1)
(defconst +copy-2-byte-offset+ 2)
(defconst +copy-4-byte-offset+ 3)       ; unused by compression

(deftype literal-length ()
  "A Snappy literal chunk length value."
 `(integer 1 #.+maximum-block-size+))

(deftype copy-length ()
  "A Snappy copy chunk length value."
 `(integer 4 #.(1- +maximum-block-size+)))

(deftype copy-offset ()
  "A Snappy copy chunk offset value."
 `(integer 1 #.(1- +maximum-block-size+)))

(defmacro postincf (variable)
  (check-type variable symbol)
  `(prog1 ,variable (incf ,variable)))

(declaim (ftype (function (uint32 hash-shift) (values hash-result &optional)) hash)
         (inline hash))

(defun hash (x shift)
  (declare (type uint32 x)
           (type hash-shift shift))
  (ash (logand (* x #x1e35a7bd) #xffffffff) (- shift)))

(declaim (ftype (function (vector-index) (values vector-index &optional))
                maximum-compressed-length))

(defun maximum-compressed-length (uncompressed-length)
  "Returns the maximum size a vector of length UNCOMPRESSED-LENGTH may take up
after it is compressed."
  (declare (type vector-index uncompressed-length))
  (+ 32 uncompressed-length (floor uncompressed-length 6)))

(declaim (ftype (function (octet-vector vector-index vector-index) (values snappy-index &optional))
                uncompressed-length))

(defun uncompressed-length (buffer index limit)
  "Returns the uncompressed length of the compressed data stored in BUFFER from
position INDEX to LIMIT."
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (multiple-value-bind (length in)
      (parse-uint64-carefully buffer index limit)
    (declare (ignore in))
    (check-type length snappy-index)
    length))

(declaim (ftype (function (octet-vector vector-index octet-vector vector-index literal-length)
                          (values vector-index &optional))
                emit-literal))

(defun emit-literal (input-buffer literal output-buffer out length)
  (declare (type octet-vector input-buffer output-buffer)
           (type vector-index literal out)
           (type literal-length length))
  (let ((n (1- length)))                ; no literal has length zero
    (declare (type vector-index n))
    (cond ((< n 60)
           (setf (aref output-buffer (postincf out)) (logior +literal+ (ash n 2))))
          ((< n #.(ash 1 8))
           (setf (aref output-buffer (postincf out)) (logior +literal+ #.(ash 60 2)))
           (setf (aref output-buffer (postincf out)) n))
          (t
           (setf (aref output-buffer (postincf out)) (logior +literal+ #.(ash 61 2)))
           (setf (aref output-buffer (postincf out)) (ldb (byte 8 0) n))
           (setf (aref output-buffer (postincf out)) (ldb (byte 8 8) n))))
    (replace output-buffer input-buffer
             :start1 out :end1 (+ out length)
             :start2 literal :end2 (+ literal length))
    (the vector-index (+ out length))))

(declaim (ftype (function (octet-vector vector-index copy-offset copy-length)
                          (values vector-index &optional))
                emit-copy))

(defun emit-copy (output-buffer out offset length)
  (declare (type octet-vector output-buffer)
           (type vector-index out)
           (type copy-offset offset)
           (type copy-length length))
  (loop while (>= length 68) do
    ;; Length 64 copy, encoded in 3 octets.
    (setf (aref output-buffer (postincf out)) (logior +copy-2-byte-offset+ #.(ash 63 2)))
    (setf (aref output-buffer (postincf out)) (ldb (byte 8 0) offset))
    (setf (aref output-buffer (postincf out)) (ldb (byte 8 8) offset))
    (decf length 64))
  (when (> length 64)
    ;; Length 60 copy, encoded in 3 octets.
    (setf (aref output-buffer (postincf out)) (logior +copy-2-byte-offset+ #.(ash 59 2)))
    (setf (aref output-buffer (postincf out)) (ldb (byte 8 0) offset))
    (setf (aref output-buffer (postincf out)) (ldb (byte 8 8) offset))
    (decf length 60))
  (when (or (>= length 12) (>= offset 2048))
    ;; Remaining copy encoded in 3 octets.
    (setf (aref output-buffer (postincf out)) (logior +copy-2-byte-offset+ (ash (1- length) 2)))
    (setf (aref output-buffer (postincf out)) (ldb (byte 8 0) offset))
    (setf (aref output-buffer (postincf out)) (ldb (byte 8 8) offset))
    (return-from emit-copy out))
  ;; Remaining copy encoded in 2 octets.
  (setf (aref output-buffer (postincf out))
        (logior +copy-1-byte-offset+
                (ash (- length 4) 2)
                (ash (ldb (byte 8 8) offset) 5)))
  (setf (aref output-buffer (postincf out)) (ldb (byte 8 0) offset))
  out)

(declaim (ftype (function (octet-vector vector-index vector-index
                           octet-vector vector-index)
                          (values vector-index &optional))
                raw-compress))

(defun raw-compress (input-buffer in in-limit output-buffer out)
  (declare (type octet-vector input-buffer output-buffer)
           (type vector-index in in-limit out))
  (let* ((in-length (- in-limit in))
         (shift +maximum-hash-shift+)
         (table-size (loop for size of-type table-size = +minimum-hash-table-size+ then (* 2 size)
                           while (and (< size +maximum-hash-table-size+) (< size in-length))
                           do (decf shift)
                           finally (return size)))
         (table (make-array table-size :element-type 'uint16 :initial-element 0))
         (orig-in in)
         (next-emit (postincf in))
         (next-hash (hash (ub32ref/le input-buffer in) shift))
         (safe-in-limit (- in-limit +input-margin+)))
    (declare (type hash-shift shift))

    (block compress-loop
      (loop
        (let ((skip 32)
              (next-in in)
              (candidate 0))
          (declare (type vector-index skip))
          (loop
            (setf in next-in)
            (let ((bytes-between-hash-lookups (ash skip -5)))
              (declare (type uint16 bytes-between-hash-lookups))
              (setf next-in (+ in bytes-between-hash-lookups))
              (incf skip bytes-between-hash-lookups)
              (when (> next-in safe-in-limit)
                (return-from compress-loop))
              (setf candidate (+ orig-in (aref table next-hash))
                    (aref table next-hash) (- in orig-in)
                    next-hash (hash (ub32ref/le input-buffer next-in) shift))
              (when (= (ub32ref/le input-buffer in) (ub32ref/le input-buffer candidate))
                (return))))

          (setf out (emit-literal input-buffer next-emit output-buffer out (- in next-emit)))

          (loop
            (let ((base in))
              (incf in 4)
              (loop for i upfrom (+ candidate 4)
                    while (and (< in in-limit) (= (aref input-buffer i) (aref input-buffer in)))
                    do (incf in))
              (setf out (emit-copy output-buffer out (- base candidate) (- in base)))
              (setf next-emit in)
              (when (>= in safe-in-limit)
                (return-from compress-loop))

              (let* ((x (ub64ref/le input-buffer (1- in)))
                     (previous-hash (hash (ldb (byte 32 0) x) shift)))
                (setf (aref table previous-hash) (- in orig-in 1))
                (let ((current-hash (hash (ldb (byte 32 8) x) shift)))
                  (setf candidate (+ orig-in (aref table current-hash))
                        (aref table current-hash) (- in orig-in))
                  (when (/= (ldb (byte 32 8) x) (ub32ref/le input-buffer candidate))
                    (setf next-hash (hash (ldb (byte 32 16) x) shift))
                    (incf in)
                    (return)))))))))

    (when (< next-emit in-limit)
      (setf out (emit-literal input-buffer next-emit output-buffer out (- in-limit next-emit))))

    out))

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
  (let* ((input-length (- limit index))
         (output-length (maximum-compressed-length input-length)))
    (when (> input-length +maximum-snappy-index+)
      (error "data size too large"))
    (when (> output-length +maximum-snappy-index+)
      (error "compressed size too large"))
    (let* ((compressed (make-octet-vector output-length))
           (out (encode-uint32 compressed 0 input-length)))
      (loop while (plusp input-length) do
        (let* ((amount (min input-length +maximum-block-size+))
               (limit (+ index amount)))
          (setf out
                (if (< amount +minimum-non-literal-block-size+)
                    (emit-literal buffer index compressed out amount)
                    (raw-compress buffer index limit compressed out)))
          (incf index amount)
          (decf input-length amount)))
      (values compressed out))))

(declaim (ftype (function (octet-vector vector-index vector-index
                           octet-vector vector-index vector-index)
                          (values vector-index &optional))
                raw-uncompress))

(defun raw-uncompress (input-buffer in in-limit output-buffer out out-limit)
  (declare (type octet-vector input-buffer output-buffer)
           (type vector-index in in-limit out out-limit))
  (let ((initial-out out))
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
             (unless (< in in-limit)
               (error "input buffer exhausted 2"))
             (let ((offset (aref input-buffer in))
                   (length 0))
               (incf in)
               (ecase opcode
                 ((#.+copy-1-byte-offset+)
                  (setf length (+ (ldb (byte 3 2) octet) 4))
                  (setf (ldb (byte 3 8) offset) (ldb (byte 3 5) octet)))
                 ((#.+copy-2-byte-offset+)
                  (unless (< in in-limit)
                    (error "input buffer exhausted 3"))
                  (setf length (1+ (ldb (byte 6 2) octet)))
                  (setf (ldb (byte 8 8) offset) (aref input-buffer in))
                  (incf in))
                 ((#.+copy-4-byte-offset+)
                  (unless (< (+ in 2) in-limit)
                    (error "input buffer exhausted 4"))
                  (setf length (1+ (ldb (byte 6 2) octet)))
                  (setf (ldb (byte 8 8) offset) (aref input-buffer in))
                  (incf in)
                  (setf (ldb (byte 8 16) offset) (aref input-buffer in))
                  (incf in)
                  (setf (ldb (byte 8 24) offset) (aref input-buffer in))
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
                  (error "output buffer too small 2"))
                (replace output-buffer input-buffer
                         :start1 out :end1 (+ out length)
                         :start2 in :end2 (+ in length))
                (incf in length)
                (incf out length))
              ;; Copy from an earlier position in output-buffer.
              (multiple-value-bind (offset length)
                  (copy-parameters octet opcode)
                (when (zerop offset)
                  (error "invalid offset 1"))
                (let ((copy (- out offset)))
                  (when (< copy initial-out)
                    (error "invalid offset 2"))
                  (when (> (+ out length) out-limit)
                    (error "output buffer too small 3"))
                  ;; Source and destination may overlap.
                  (loop repeat length do
                    (setf (aref output-buffer out) (aref output-buffer copy))
                    (incf out)
                      (incf copy)))))))))
  (unless (= out out-limit)
    (error "bad decompressed length"))
  out)

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values octet-vector &optional))
                uncompress))

(defun uncompress (buffer index limit)
  "Uncompresses BUFFER, a vector of (UNSIGNED-BYTE 8), from position INDEX to
LIMIT.  Returns the uncompressed data as a vector of (UNSIGNED-BYTE 8)."
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (multiple-value-bind (length in)
      (parse-uint64-carefully buffer index limit)
    (check-type length snappy-index)
    (let ((uncompressed (make-octet-vector length)))
      (raw-uncompress buffer in limit uncompressed 0 length)
      uncompressed)))


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
