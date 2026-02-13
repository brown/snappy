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

;;;; Test snappy compression.

(in-package #:common-lisp-user)

(defpackage #:snappy-test
  (:documentation "Test code in the SNAPPY package.")
  (:use #:common-lisp
        #:hu.dwim.stefil
        #:snappy)
  (:import-from #:acm-random #:acm-random)
  (:import-from #:com.google.base
                #:make-octet-vector
                #:octet
                #:octet-vector
                #:uint32
                #:uint64
                #:string-to-utf8-octets
                #:utf8-octets-to-string
                #:vector-index)
  (:import-from #:nibbles
                #:ub32ref/le
                #:ub64ref/le)
  (:import-from #:random
                #:next-uint32
                #:next-uint8
                #:one-in
                #:skewed-uint32
                #:uniform-uint32)
  (:import-from #:varint
                #:data-exhausted
                #:encode-uint32-carefully
                #:parse-uint32-carefully
                #:value-out-of-range)
  (:export #:read-data-file
           #:test-snappy))

(in-package #:snappy-test)

(defsuite (test-snappy :in root-suite) ()
  (run-child-tests))

(in-suite test-snappy)

(defun verify-octets (octets)
  "Verifies that OCTETS, a vector of octets, compresses to a size we expect, and
that the compressed copy produces OCTETS when uncompressed."
  (multiple-value-bind (compressed compressed-length)
      (compress octets 0 (length octets))
    (is (<= compressed-length (maximum-compressed-length (length octets))))
    (let ((uncompressed (uncompress compressed 0 compressed-length)))
      (is (not (mismatch octets uncompressed :test #'=))))))

(defun verify-string (string)
  "Verfies that STRING can compressed and uncompressed correctly."
  (let ((octets (string-to-utf8-octets string)))
    (is (= (length octets) (length string)))
    (verify-octets octets)))

(deftest small-strings ()
  (dolist (string '("" "a" "ab" "abc"))
    (verify-string string)))

(deftest repeated-characters ()
  (dolist (count '(16 256 2047 65536))
    (let ((repeated-b (make-string count :initial-element #\b)))
      (verify-string (concatenate 'string "aaaaaaa" repeated-b "aaaaaabc"))))
  (let ((repeated-b (make-string 65536 :initial-element #\b)))
    (verify-string (concatenate 'string "abcaaaaaaa" repeated-b "aaaaaabc"))))

(deftest max-blowup ()
  "Tests maximum compressed string blowup by maximizing four-byte copies."
  (let ((input (make-octet-vector (* 2 4 20000)))
        (index 0))
    (flet ((add-random-octets (i)
             (let* ((random (make-instance 'acm-random :seed i)))
               (setf (ub32ref/le input index) (next-uint32 random))
               (incf index 4))))
      (loop for i from 0 below 20000 do (add-random-octets i))
      (loop for i from 19999 downto 0 do (add-random-octets i)))
    (verify-octets input)))

(deftest small-regular ()
  (loop for size from 1 below 20000 by 23 do
    (let ((octets (make-octet-vector size)))
      (loop for i from 0 below size do
        (setf (aref octets i) (+ (mod i 10) (char-code #\a))))
      (verify-octets octets))))

(deftest small-random ()
  (loop for size from 1 below 20000 by 23 do
    (let ((octets (make-octet-vector size)))
      (loop for i from 0 below size do
        (setf (aref octets i) (random 256)))
      (verify-octets octets))))

(deftest random-buffers ()
  (let ((random (make-instance 'acm-random)))
    (dotimes (i 20000)
      (let* ((length (if (< i 100)
                         (+ 65536 (uniform-uint32 random 65536))
                         (uniform-uint32 random 4096)))
             (octets (make-octet-vector length))
             (index 0))
          (loop while (< index length) do
            (let ((run-length (if (one-in random 10) (skewed-uint32 random 8) 1))
                  (octet (if (< i 100) (uniform-uint32 random 256) (skewed-uint32 random 3))))
              (loop repeat run-length
                    while (< index length)
                    do (setf (aref octets index) octet)
                       (incf index))))
        (verify-octets octets)))))

(declaim (ftype (function (uint32) (values (integer -1 31) &optional))
                count-trailing-zeroes-uint32)
         (inline count-trailing-zeroes-uint32))

(defun count-trailing-zeroes-uint32 (n)
  (declare (type uint32 n))
  (1- (integer-length (logand n (- n)))))

(declaim (ftype (function (uint64) (values (integer -1 63) &optional))
                count-trailing-zeroes-uint64)
         (inline count-trailing-zeroes-uint64))

(defun count-trailing-zeroes-uint64 (n)
  (declare (type uint64 n))
  (1- (integer-length (logand n (- n)))))

(declaim (ftype (function (octet-vector vector-index octet-vector vector-index vector-index)
                          (values vector-index &optional))
                find-match-length-64)
         (inline find-match-length-64))

(defun find-match-length-64 (s1 s1-index s2 s2-index s2-limit)
  (declare (type octet-vector s1 s2)
           (type vector-index s1-index s2-index s2-limit))
  (let ((matched s1-index))
    (declare (type vector-index matched))
    (loop while (<= s2-index (- s2-limit 8)) do
      (let ((s2-data (ub64ref/le s2 s2-index))
            (s1-data (ub64ref/le s1 matched)))
        (if (= s2-data s1-data)
            (progn (incf s2-index 8)
                   (incf matched 8))
            (let* ((x (logxor s2-data s1-data))
                   (matching-bits (count-trailing-zeroes-uint64 x)))
              (incf matched (ash matching-bits -3))
              (return-from find-match-length-64 (- matched s1-index))))))
    (loop while (< s2-index s2-limit) do
      (if (= (aref s1 matched) (aref s2 s2-index))
          (progn (incf s2-index)
                 (incf matched))
          (return-from find-match-length-64 (- matched s1-index))))
    (- matched s1-index)))

(declaim (ftype (function (octet-vector vector-index octet-vector vector-index vector-index)
                          (values vector-index &optional))
                find-match-length-32)
         (inline find-match-length-32))

(defun find-match-length-32 (s1 s1-index s2 s2-index s2-limit)
  (declare (type octet-vector s1 s2)
           (type vector-index s1-index s2-index s2-limit))
  (let ((matched s1-index))
    (declare (type vector-index matched))
    (loop while (<= s2-index (- s2-limit 4)) do
      (unless (= (ub32ref/le s1 matched) (ub32ref/le s2 s2-index))
        (return))
      (incf s2-index 4)
      (incf matched 4))
    (if (<= s2-index (- s2-limit 4))
        (let* ((x (logxor (ub32ref/le s1 matched) (ub32ref/le s2 s2-index)))
               (matching-bits (count-trailing-zeroes-uint64 x)))
          (incf matched (ash matching-bits -3)))
        (loop while (and (< s2-index s2-limit) (= (aref s1 matched) (aref s2 s2-index))) do
          (incf s2-index)
          (incf matched)))
    (- matched s1-index)))

(deftest find-match-length ()
  (flet ((verify-match-length (s1 s2 limit)
           (let* ((s1-octets (string-to-utf8-octets (concatenate 'string s1 #(#\Null))))
                  (s2-octets (string-to-utf8-octets (concatenate 'string s2 #(#\Null))))
                  (match-32 (find-match-length-32 s1-octets 0 s2-octets 0 limit))
                  (match-64 (find-match-length-64 s1-octets 0 s2-octets 0 limit)))
             (is (= match-32 match-64))
             match-32)))

;; Generate two octet arrays of needed lengths, identical on overlap.
;; 8 bits in each octet where difference should be
;; Try all 256^2 possible differences at position.


    ;; Exercise all different code paths through the function.

    ;; 64-bit version

    ;; Hit s1_limit in 64-bit loop, hit s1_limit in single-character loop.
    (is (= 6 (verify-match-length "012345" "012345" 6)))
    (is (= 11 (verify-match-length "01234567abc" "01234567abc" 11)))

    ;; Hit s1_limit in 64-bit loop, find a non-match in single-character loop.
    (is (= 9 (verify-match-length "01234567abc" "01234567axc" 9)))

    ;; Same, but edge cases.
    (is (= 11 (verify-match-length "01234567abc!" "01234567abc!" 11)))
    (is (= 11 (verify-match-length "01234567abc!" "01234567abc?" 11)))

    ;; Find non-match at once in first loop.
    (is (= 0 (verify-match-length "01234567xxxxxxxx" "?1234567xxxxxxxx" 16)))
    (is (= 1 (verify-match-length "01234567xxxxxxxx" "0?234567xxxxxxxx" 16)))
    (is (= 4 (verify-match-length "01234567xxxxxxxx" "01237654xxxxxxxx" 16)))
    (is (= 7 (verify-match-length "01234567xxxxxxxx" "0123456?xxxxxxxx" 16)))

    ;; Find non-match in first loop after one block.
    (is (= 8 (verify-match-length "abcdefgh01234567xxxxxxxx" "abcdefgh?1234567xxxxxxxx" 24)))
    (is (= 9 (verify-match-length "abcdefgh01234567xxxxxxxx" "abcdefgh0?234567xxxxxxxx" 24)))
    (is (= 12 (verify-match-length "abcdefgh01234567xxxxxxxx" "abcdefgh01237654xxxxxxxx" 24)))
    (is (= 15 (verify-match-length "abcdefgh01234567xxxxxxxx" "abcdefgh0123456?xxxxxxxx" 24)))

    ;; 32-bit version:

    ;; Short matches.
    (is (= 0 (verify-match-length "01234567" "?1234567" 8)))
    (is (= 1 (verify-match-length "01234567" "0?234567" 8)))
    (is (= 2 (verify-match-length "01234567" "01?34567" 8)))
    (is (= 3 (verify-match-length "01234567" "012?4567" 8)))
    (is (= 4 (verify-match-length "01234567" "0123?567" 8)))
    (is (= 5 (verify-match-length "01234567" "01234?67" 8)))
    (is (= 6 (verify-match-length "01234567" "012345?7" 8)))
    (is (= 7 (verify-match-length "01234567" "0123456?" 8)))
    (is (= 7 (verify-match-length "01234567" "0123456?" 7)))
    (is (= 7 (verify-match-length "01234567!" "0123456??" 7)))

    ;; Hit s1_limit in 32-bit loop, hit s1_limit in single-character loop.
    (is (= 10 (verify-match-length "xxxxxxabcd" "xxxxxxabcd" 10)))
    (is (= 10 (verify-match-length "xxxxxxabcd?" "xxxxxxabcd?" 10)))
    (is (= 13 (verify-match-length "xxxxxxabcdef" "xxxxxxabcdef" 13)))

    ;; Same, but edge cases.
    (is (= 12 (verify-match-length "xxxxxx0123abc!" "xxxxxx0123abc!" 12)))
    (is (= 12 (verify-match-length "xxxxxx0123abc!" "xxxxxx0123abc?" 12)))

    ;; Hit s1_limit in 32-bit loop, find a non-match in single-character loop.
    (is (= 11 (verify-match-length "xxxxxx0123abc" "xxxxxx0123axc" 13)))

    ;; Find non-match at once in first loop.
    (is (= 6 (verify-match-length "xxxxxx0123xxxxxxxx" "xxxxxx?123xxxxxxxx" 18)))
    (is (= 7 (verify-match-length "xxxxxx0123xxxxxxxx" "xxxxxx0?23xxxxxxxx" 18)))
    (is (= 8 (verify-match-length "xxxxxx0123xxxxxxxx" "xxxxxx0132xxxxxxxx" 18)))
    (is (= 9 (verify-match-length "xxxxxx0123xxxxxxxx" "xxxxxx012?xxxxxxxx" 18)))

    ;; Same, but edge cases.
    (is (= 6 (verify-match-length "xxxxxx0123" "xxxxxx?123" 10)))
    (is (= 7 (verify-match-length "xxxxxx0123" "xxxxxx0?23" 10)))
    (is (= 8 (verify-match-length "xxxxxx0123" "xxxxxx0132" 10)))
    (is (= 9 (verify-match-length "xxxxxx0123" "xxxxxx012?" 10)))

    ;; Find non-match in first loop after one block.
    (is (= 10 (verify-match-length "xxxxxxabcd0123xx" "xxxxxxabcd?123xx" 16)))
    (is (= 11 (verify-match-length "xxxxxxabcd0123xx" "xxxxxxabcd0?23xx" 16)))
    (is (= 12 (verify-match-length "xxxxxxabcd0123xx" "xxxxxxabcd0132xx" 16)))
    (is (= 13 (verify-match-length "xxxxxxabcd0123xx" "xxxxxxabcd012?xx" 16)))

    ;; Same, but edge cases.
    (is (= 10 (verify-match-length "xxxxxxabcd0123" "xxxxxxabcd?123" 14)))
    (is (= 11 (verify-match-length "xxxxxxabcd0123" "xxxxxxabcd0?23" 14)))
    (is (= 12 (verify-match-length "xxxxxxabcd0123" "xxxxxxabcd0132" 14)))
    (is (= 13 (verify-match-length "xxxxxxabcd0123" "xxxxxxabcd012?" 14)))
    ))

(deftest max-block-size-maximum-compressed-length ()
  (is (= 76490 (maximum-compressed-length 65536))))

(deftest zero-length-vector ()
  (verify-octets (make-octet-vector 0)))

(deftest small-copy ()
  (dotimes (count 32)
    (let ((repeated-b (make-string count :initial-element #\b)))
      (verify-string (concatenate 'string "aaaa" repeated-b "aaaabbbb")))))

(deftest uncompressed-length-varint ()
  ;; Varint parsing should signal either that the final octet has the continuation bit set or that
  ;; the data buffer is exhausted.
  (let ((octets (make-octet-vector 1 :initial-contents '(#xff))))
    (signals data-exhausted (uncompressed-length octets 0 1)))
  ;; Varint value overflows a uint64.
  (let* ((input '(#xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #x00))
         (octets (make-octet-vector (length input) :initial-contents input)))
    (signals value-out-of-range (uncompressed-length octets 0 (length octets))))
  ;; Snappy documentation says the maximum uncompressed buffer size is 2^32 - 1.
  ;; https://github.com/google/snappy/blob/master/format_description.txt
  (let ((octets (make-octet-vector 5 :initial-contents '(#x80 #x80 #x80 #x80 #x10))))
    (signals error (uncompressed-length octets 0 5))))

(deftest decode ()
  (let ((test-cases
          `(;; len=0; valid input
            ((#x00) () nil)
            ;; len=3; literal, 0-byte length; length=3; valid input
            ((#x03 #x08 #xff #xff #xff) (#xff #xff #xff) nil)
            ;; len=2; literal, 0-byte length; length=3; not enough dst bytes
            ((#x02 #x08 #xff #xff #xff) () error)
            ;; len=3; literal, 0-byte length; length=3; not enough src bytes
            ((#x03 #x08 #xff #xff) () error)
            ;; len=40; literal, 0-byte length; length=40; valid input
	    ((#x28 #x9c ,@(loop for i below 40 collect i)) ,(loop for i below 40 collect i) nil)
            ;; len=1; literal, 1-byte length; not enough length bytes
            ((#x01 #xf0) () error)
            ;; len=3; literal, 1-byte length; length=3; valid input
            ((#x03 #xf0 #x02 #xff #xff #xff) (#xff #xff #xff) nil)
            ;; len=1; literal, 2-byte length; not enough length bytes
	    ((#x01 #xf4 #x00) () error)
            ;; len=3; literal, 2-byte length; length=3; valid input
            ((#x03 #xf4 #x02 #x00 #xff #xff #xff) (#xff #xff #xff) nil)
            ;; len=1; literal, 3-byte length; not enough length bytes
	    ((#x01 #xf8 #x00 #x00) () error)
            ;; len=3; literal, 3-byte length; length=3; valid input
	    ((#x03 #xf8 #x02 #x00 #x00 #xff #xff #xff) (#xff #xff #xff) nil)
            ;; len=1; literal, 4-byte length; not enough length bytes
	    ((#x01 #xfc #x00 #x00 #x00) () error)
            ;; len=1; literal, 4-byte length; length=3; not enough dst bytes
            ((#x01 #xfc #x02 #x00 #x00 #x00 #xff #xff #xff) () error)
            ;; len=4; literal, 4-byte length; length=3; not enough src bytes
            ((#x04 #xfc #x02 #x00 #x00 #x00 #xff) () error)
            ;; len=3; literal, 4-byte length; length=3; valid input
	    ((#x03 #xfc #x02 #x00 #x00 #x00 #xff #xff #xff) (#xff #xff #xff) nil)
            ;; len=4; copy1, 1 extra length|offset byte; not enough extra bytes
	    ((#x04 #x01) () error)
            ;; len=4; copy2, 2 extra length|offset bytes; not enough extra bytes
            ((#x04 #x02 #x00) () error)
            ;; len=4; copy4, 4 extra length|offset bytes; not enough extra bytes
            ((#x04 #x03 #x00 #x00 #x00) () error)
            ;; len=4; literal (4 bytes "abcd"); valid input
	    ((#x04 #x0c #\a #\b #\c #\d) (#\a #\b #\c #\d) nil)
            ;; len=13; literal (4 bytes "abcd"); copy1; length=9 offset=4; valid input
	    ((#x0d #x0c #\a #\b #\c #\d #x15 #x04)
             (#\a #\b #\c #\d #\a #\b #\c #\d #\a #\b #\c #\d #\a)
             nil)
            ;; len=8; literal (4 bytes "abcd"); copy1; length=4 offset=4; valid input
	    ((#x08 #x0c #\a #\b #\c #\d #x01 #x04) (#\a #\b #\c #\d #\a #\b #\c #\d) nil)
            ;; len=8; literal (4 bytes "abcd"); copy1; length=4 offset=2; valid input
	    ((#x08 #x0c #\a #\b #\c #\d #x01 #x02) (#\a #\b #\c #\d #\c #\d #\c #\d) nil)
            ;; len=8; literal (4 bytes "abcd"); copy1; length=4 offset=1; valid input
	    ((#x08 #x0c #\a #\b #\c #\d #x01 #x01) (#\a #\b #\c #\d #\d #\d #\d #\d) nil)
            ;; len=8; literal (4 bytes "abcd"); copy1; length=4 offset=0; zero offset
	    ((#x08 #x0c #\a #\b #\c #\d #x01 #x00) () error)
            ;; len=9; literal (4 bytes "abcd"); copy1; length=4 offset=4; len mismatch
            ((#x09 #x0c #\a #\b #\c #\d #x01 #x04) () error)
            ;; len=8; literal (4 bytes "abcd"); copy1; length=4 offset=5; offset too large
	    ((#x08 #x0 #\c #\a #\b #\c #\d #x01 #x05) () error)
            ;; len=7; literal (4 bytes "abcd"); copy1; length=4 offset=4; length too large
            ((#x07 #x0c #\a #\b #\c #\d #x01 #x04) () error)
            ;; len=6; literal (4 bytes "abcd"); copy2; length=2 offset=3; valid input
	    ((#x06 #x0c #\a #\b #\c #\d #x06 #x03 #x00) (#\a #\b #\c #\d #\b #\c) nil)
            ;; len=6; literal (4 bytes "abcd"); copy4; length=2 offset=3; valid input
	    ((#x06 #x0c #\a #\b #\c #\d #x07 #x03 #x00 #x00 #x00) (#\a #\b #\c #\d #\b #\c) nil)
            ;; len=0; copy4, 4 length/offset bytes with MSB 0x93
	    ((#x00 #xfc #\0 #\0 #\0 #x93) nil error)))
        (sentinal-base #xa0)
        (sentinal-length 37)
        (uncompressed (make-octet-vector 100)))
    (loop for (raw-input raw-want error) in test-cases do
      (let ((input (loop for i in raw-input collect (if (numberp i) i (char-code i))))
            (want (loop for i in raw-want collect (if (numberp i) i (char-code i)))))

        ;; Sentinal octets do not occur in the data to be uncompressed.
        (loop for octet in input do
          (assert (not (<= sentinal-base octet (1- (+ sentinal-base sentinal-length))))))

        (let ((compressed (make-octet-vector (length input) :initial-contents input)))
          (multiple-value-bind (length in)
              (parse-uint32-carefully compressed 0 (length compressed))

            ;; Uncompressed data will fit in the buffer.
            (assert (<= 0 length (1- (length uncompressed))))

            ;; Write sentinal octets to the entire buffer.
            (loop for i below (length uncompressed) do
              (setf (aref uncompressed i) (+ sentinal-base (mod i sentinal-length))))

            (ecase error
              ((error)
               (signals error
                 (snappy::raw-uncompress compressed in (length compressed)
                                         uncompressed 0 length)))

              ((nil foobar)
               (let ((uncompressed-length
                       (snappy::raw-uncompress compressed in (length compressed)
                                               uncompressed 0 length))
                     (want-octets (make-octet-vector (length want) :initial-contents want)))
                 (is (not (mismatch uncompressed want-octets :end1 uncompressed-length))))))

            ;; No octets outside the uncompressed data were modified.
            (loop for i upfrom length below (length uncompressed) do
              (is (= (aref uncompressed i) (+ sentinal-base (mod i sentinal-length)))))))))))

(deftest decode-copy-4 ()
  (let* ((dots (make-string 65536 :initial-element #\.))
         (input `(;; decoded length is 65545
                  #x89 #x80 #x04
                  ;; 4-byte literal "pqrs"
                  #x0c #\p #\q #\r #\s
                  ;; 65536-byte literal dots
                  #xf4 #xff #xff ,@(loop for ch across dots collect ch)
                  ;; copy 4 with length 5 and offset 65540
                  #x13 #x04 #x00 #x01 #x00))
         (octets (loop for i in input collect (if (numberp i) i (char-code i))))
         (compressed (make-octet-vector (length octets) :initial-contents octets))
         (uncompressed (uncompress compressed 0 (length compressed)))
         (got (utf8-octets-to-string uncompressed))
         (want (concatenate 'string "pqrs" dots "pqrs.")))
    (is (string= got want))))

(deftest decode-length-offset ()
  (let ((prefix (string-to-utf8-octets "abcdefghijklmnopqr"))
        (suffix (string-to-utf8-octets "ABCDEFGHIJKLMNOPQR"))
        (sentinal-base #xa0)
        (sentinal-length 37)
        (input (make-octet-vector 128))
        (got (make-octet-vector 128))
        (want (make-octet-vector 128)))
    (loop for length from 1 upto 18 do
      (loop for offset from 1 upto 18 do
        (loop for suffix-length upto 18 do
          (let* ((total-length (+ (length prefix) length suffix-length))
                 (in (encode-uint32-carefully input 0 (length input) total-length))
                 (input-index in))
            (setf (aref input input-index) (+ snappy::+literal+ (* 4 (1- (length prefix)))))
            (incf input-index)
            (replace input prefix :start1 input-index)
            (incf input-index (length prefix))
            (setf (aref input input-index) (+ snappy::+copy-2-byte-offset+ (* 4 (1- length))))
            (incf input-index)
            (setf (aref input input-index) offset)
            (incf input-index)
            (setf (aref input input-index) #x00)
            (incf input-index)
            (when (plusp suffix-length)
              (setf (aref input input-index) (+ snappy::+literal+ (* 4 (1- suffix-length))))
              (incf input-index)
              (replace input suffix :start1 input-index :end2 suffix-length)
              (incf input-index suffix-length))

            ;; Sentinal octets do not occur in the data to be uncompressed.
            (loop for i below input-index do
              (assert (not (<= sentinal-base
                               (aref input i)
                               (1- (+ sentinal-base sentinal-length))))))

            ;; Write sentinal octets to the entire got buffer.
            (loop for i below (length got) do
              (setf (aref got i) (+ sentinal-base (mod i sentinal-length))))

            (let ((got-length (snappy::raw-uncompress input in input-index got 0 total-length)))
              ;; No octets outside the uncompressed data were modified.
              (loop for i from total-length below (length got) do
                (is (= (aref got i) (+ sentinal-base (mod i sentinal-length)))))

              (let ((want-index 0))
                (replace want prefix)
                (incf want-index (length prefix))
                (loop repeat length do
                  (setf (aref want want-index) (aref want (- want-index offset)))
                  (incf want-index))
                (replace want suffix :start1 want-index :end2 suffix-length)
                (incf want-index suffix-length)
                ;; Sentinal octets to not occur in the wanted data.
                (loop for i below want-index do
                  (assert (not (<= sentinal-base
                                   (aref want i)
                                   (1- (+ sentinal-base sentinal-length))))))

                ;; Uncompressed data must match what we expect.
                (is (not (mismatch got want :test #'= :end1 got-length :end2 want-index)))))))))))

(defun read-data-file (file-name &optional limit)
  (let ((data-pathname
          (asdf/system:system-relative-pathname 'snappy
                                                (concatenate 'string "testdata/" file-name))))
    (with-open-file (stream data-pathname :element-type 'octet :if-does-not-exist :error)
      (let* ((octet-count (if limit limit (file-length stream)))
             (data (make-octet-vector octet-count))
             (octets-read (read-sequence data stream)))
        (assert (= octets-read octet-count))
        data))))

(deftest uncompress-golden ()
  (let ((golden-compressed (read-data-file "Isaac.Newton-Opticks.txt.rawsnappy"))
        (golden (read-data-file "Isaac.Newton-Opticks.txt")))
    (let ((uncompressed (uncompress golden-compressed 0 (length golden-compressed))))
      (is (not (mismatch uncompressed golden :test #'=))))))

#+nil                                   ; currently fails!
(deftest compress-golden ()
  (let ((golden (read-data-file "Isaac.Newton-Opticks.txt"))
        (golden-compressed (read-data-file "Isaac.Newton-Opticks.txt.rawsnappy")))
    (multiple-value-bind (compressed compressed-length)
        (compress golden 0 (length golden))
      (declare (ignore compressed-length))
      (is (not (mismatch compressed golden-compressed :test #'=))))))

(deftest encode-noise-then-repeats ()
  "Compresses data for which the first half is very incompressible and the second half is very
compressible. The length of the compressed data should be closer to 50% of the original length than
100%."
  (loop for length in (list (* 256 1024) (* 2048 1024)) do
    (let ((octets (make-octet-vector length))
          (random (make-instance 'acm-random :seed 1))
          (middle (floor length 2)))
      (loop for i below middle do (setf (aref octets i) (next-uint8 random)))
      (loop for i from middle below length do (setf (aref octets i) (ldb (byte 8 8) (- i middle))))
      (multiple-value-bind (compressed compressed-length)
          (compress octets 0 (length octets))
        (declare (ignore compressed))
        (is (< compressed-length (floor (* 3 length) 4)))))))

(deftest emit-literal ()
  (let ((test-cases '((1 (#x00))
		      (2 (#x04))
		      (59 (#xe8))
		      (60 (#xec))
		      (61 (#xf0 #x3c))
		      (62 (#xf0 #x3d))
		      (254 (#xf0 #xfd))
		      (255 (#xf0 #xfe))
		      (256 (#xf0 #xff))
		      (257 (#xf4 #x00 #x01))
		      (65534 (#xf4 #xfd #xff))
		      (65535 (#xf4 #xfe #xff))
		      (65536 (#xf4 #xff #xff))))
        (dest (make-octet-vector 70000)))
    (loop for (length want-octets) in test-cases do
      (let* ((want (make-octet-vector (length want-octets) :initial-contents want-octets))
             (literal (make-array length :element-type 'octet :initial-element #x99))
             (out (snappy::emit-literal literal 0 dest 0 length)))
        (is (not (mismatch dest literal :start1 (length want) :end1 out)))
        (is (not (mismatch dest want :end1 (length want))))))))

(deftest emit-copy ()
  (let ((test-cases '((8 04 (#x01 #x08))
		      (8 11 (#x1d #x08))
		      (8 12 (#x2e #x08 #x00))
		      (8 13 (#x32 #x08 #x00))
		      (8 59 (#xea #x08 #x00))
		      (8 60 (#xee #x08 #x00))
		      (8 61 (#xf2 #x08 #x00))
		      (8 62 (#xf6 #x08 #x00))
		      (8 63 (#xfa #x08 #x00))
		      (8 64 (#xfe #x08 #x00))
		      (8 65 (#xee #x08 #x00 #x05 #x08))
		      (8 66 (#xee #x08 #x00 #x09 #x08))
		      (8 67 (#xee #x08 #x00 #x0d #x08))
		      (8 68 (#xfe #x08 #x00 #x01 #x08))
		      (8 69 (#xfe #x08 #x00 #x05 #x08))
		      (8 80 (#xfe #x08 #x00 #x3e #x08 #x00))

		      (256 04 (#x21 #x00))
		      (256 11 (#x3d #x00))
		      (256 12 (#x2e #x00 #x01))
		      (256 13 (#x32 #x00 #x01))
		      (256 59 (#xea #x00 #x01))
		      (256 60 (#xee #x00 #x01))
		      (256 61 (#xf2 #x00 #x01))
		      (256 62 (#xf6 #x00 #x01))
		      (256 63 (#xfa #x00 #x01))
		      (256 64 (#xfe #x00 #x01))
		      (256 65 (#xee #x00 #x01 #x25 #x00))
		      (256 66 (#xee #x00 #x01 #x29 #x00))
		      (256 67 (#xee #x00 #x01 #x2d #x00))
		      (256 68 (#xfe #x00 #x01 #x21 #x00))
		      (256 69 (#xfe #x00 #x01 #x25 #x00))
		      (256 80 (#xfe #x00 #x01 #x3e #x00 #x01))

		      (2048 04 (#x0e #x00 #x08))
		      (2048 11 (#x2a #x00 #x08))
		      (2048 12 (#x2e #x00 #x08))
		      (2048 13 (#x32 #x00 #x08))
		      (2048 59 (#xea #x00 #x08))
		      (2048 60 (#xee #x00 #x08))
		      (2048 61 (#xf2 #x00 #x08))
		      (2048 62 (#xf6 #x00 #x08))
		      (2048 63 (#xfa #x00 #x08))
		      (2048 64 (#xfe #x00 #x08))
		      (2048 65 (#xee #x00 #x08 #x12 #x00 #x08))
		      (2048 66 (#xee #x00 #x08 #x16 #x00 #x08))
		      (2048 67 (#xee #x00 #x08 #x1a #x00 #x08))
		      (2048 68 (#xfe #x00 #x08 #x0e #x00 #x08))
		      (2048 69 (#xfe #x00 #x08 #x12 #x00 #x08))
		      (2048 80 (#xfe #x00 #x08 #x3e #x00 #x08))))
        (dest (make-array 1024 :element-type 'octet :initial-element 0)))
    (loop for (offset length want-octets) in test-cases do
      (let* ((want (make-octet-vector (length want-octets) :initial-contents want-octets))
             (out (snappy::emit-copy dest 0 offset length)))
        (is (not (mismatch dest want :end1 out)))))))
