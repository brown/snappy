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

;;;; Author: brown@google.com (Robert Brown)

;;;; Test snappy compression.

(in-package #:common-lisp-user)

(defpackage #:snappy-test
  (:documentation "Test code in the SNAPPY package.")
  (:use #:common-lisp
        #:com.google.base
        #:hu.dwim.stefil
        #:snappy)
  (:export #:test-snappy))

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
             (let* ((random (make-instance 'acm-random:acm-random :seed i)))
               (setf (nibbles:ub32ref/le input index) (random:next-uint32 random))
               (incf index 4))))
      (loop for i from 0 below 20000 do (add-random-octets i))
      (loop for i from 19999 downto 0 do (add-random-octets i)))
    (verify-octets input)))

(deftest small-regular ()
  (loop for size from 1 below 20000 by 23 do
    (let ((octets (make-octet-vector size)))
      (loop for i from 0 below size do
        (setf (aref octets i) (+ (mod i 10) 97)))
      (verify-octets octets))))

(deftest small-random ()
  (loop for size from 1 below 20000 by 23 do
    (let ((octets (make-octet-vector size)))
      (loop for i from 0 below size do
        (setf (aref octets i) (random 256)))
      (verify-octets octets))))

(deftest random-buffers ()
  (let ((random (make-instance 'acm-random:acm-random)))
    (dotimes (i 20000)
      (let* ((length (if (< i 100)
                         (+ 65536 (random:uniform-uint32 random 65536))
                         (random:uniform-uint32 random 4096)))
             (octets (make-octet-vector length))
             (index 0))
          (loop while (< index length) do
            (let ((run-length (if (random:one-in random 10)
                                  (random:skewed-uint32 random 8)
                                  1))
                  (octet (if (< i 100)
                             (random:uniform-uint32 random 256)
                             (random:skewed-uint32 random 3))))
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
      (let ((s2-data (nibbles:ub64ref/le s2 s2-index))
            (s1-data (nibbles:ub64ref/le s1 matched)))
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
      (unless (= (nibbles:ub32ref/le s1 matched) (nibbles:ub32ref/le s2 s2-index))
        (return))
      (incf s2-index 4)
      (incf matched 4))
    (if (<= s2-index (- s2-limit 4))
        (let* ((x (logxor (nibbles:ub32ref/le s1 matched) (nibbles:ub32ref/le s2 s2-index)))
               (matching-bits (count-trailing-zeroes-uint64 x)))
          (incf matched (ash matching-bits -3)))
        (loop while (and (< s2-index s2-limit) (= (aref s1 matched) (aref s2 s2-index))) do
          (incf s2-index)
          (incf matched)))
    (- matched s1-index)))

(deftest test-find-match-length ()
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
