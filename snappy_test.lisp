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
(declaim #.*optimize-default*)

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
             (let* ((random (make-instance 'acm-random:acm-random :seed i))
                    (octets (random:next-uint32 random)))
               (setf (aref input index) (ldb (byte 8 0) octets))
               (incf index)
               (setf (aref input index) (ldb (byte 8 8) octets))
               (incf index)
               (setf (aref input index) (ldb (byte 8 16) octets))
               (incf index)
               (setf (aref input index) (ldb (byte 8 24) octets))
               (incf index))))
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
