;;;; Copyright (c) 2026 Robert E. Brown. All rights reserved.

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;    * Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;;    * Redistributions in binary form must reproduce the above
;;;; copyright notice, this list of conditions and the following disclaimer
;;;; in the documentation and/or other materials provided with the
;;;; distribution.
;;;;    * Neither the name of Google Inc. nor the names of its
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

;;;; Simple benchmarking similar to the Go and C++ benchmarking libraries.

(in-package #:common-lisp-user)

(defpackage #:benchmark
  (:use #:common-lisp)
  (:import-from #:cl-cpus #:get-number-of-processors)
  (:export #:define-benchmark
           #:benchmark-loop
           #:reset-timer
           #:run-benchmarks
           #:set-data-size))

(in-package #:benchmark)

(defvar *benchmarks* ())

(defmacro define-benchmark (benchmark-function display-name (bench) &body body)
  "Defines a benchmark function and registers it with a specific display name."
  `(progn
     (defun ,benchmark-function (,bench) ,@body)
     ;; Register function symbol and the human-readable string name
     (setf *benchmarks* (append (remove ',benchmark-function *benchmarks* :key #'car)
                                (list (list ',benchmark-function ,display-name))))
     ',benchmark-function))

(defmacro benchmark-loop ((benchmark) &body body)
  `(loop repeat (iteration-count ,benchmark) do
         ,@body))

(defclass benchmark ()
  ((name :initarg :name
         :reader benchmark-name)
   (iteration-count :initarg :iteration-count
                    :reader iteration-count)
   (start-time :accessor start-time
               :initform (get-internal-real-time))
   (data-size :accessor data-size
              :initform 0)))

(defgeneric reset-timer (benchmark)
  (:method ((benchmark benchmark))
    (setf (start-time benchmark) (get-internal-real-time))))

(defgeneric set-data-size (benchmark size)
  (:method ((benchmark benchmark) size)
    (setf (data-size benchmark) size)))

(defun format-bandwidth (bytes-per-op ns-per-op)
  (if (zerop bytes-per-op)
      ""
      (format nil "~10,2F MB/s" (/ (* bytes-per-op 1e9) (* ns-per-op 1024 1024)))))

(defun report-result (benchmark-name iterations duration total-bytes)
  (let ((cpu-count (get-number-of-processors))
        (ns-per-op (* (/ duration iterations) 1e9))
        (bytes-per-op (float (/ total-bytes iterations))))
    (format t "~&~A-~D~28T~10D~40T~10,2F ns/op~55T~A"
            benchmark-name cpu-count iterations ns-per-op
            (format-bandwidth bytes-per-op ns-per-op))))

(defun run-benchmarks ()
  (dolist (entry *benchmarks*)
    (destructuring-bind (benchmark-function benchmark-name) entry
      (let ((iterations 1))
        (loop
          (let ((bench (make-instance 'benchmark :iteration-count iterations :name benchmark-name)))
            (funcall benchmark-function bench)
            (let* ((end (get-internal-real-time))
                   (duration (float (/ (- end (start-time bench)) internal-time-units-per-second)))
                   (size (* iterations (data-size bench))))
              (if (or (>= duration 1.0) (>= iterations 1000000000))
                  (return (report-result benchmark-name iterations duration size))
                  (setf iterations
                        (max (1+ iterations)
                             (truncate (* iterations (/ 1.2 (max duration 0.001)))))))))))))
  (values))
