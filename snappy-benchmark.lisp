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

;;;; Benchmark Snappy compression.

(in-package #:common-lisp-user)

(defpackage #:snappy-benchmark
  (:documentation "Benchmark code in the SNAPPY package.")
  (:use #:common-lisp
        #:benchmark
        #:snappy)
  (:import-from #:acm-random #:acm-random)
  (:import-from #:com.google.base
                #:make-octet-vector
                #:octet)
  (:import-from #:random #:next-uint8)
  (:import-from #:snappy-test #:read-data-file)
  (:export #:benchmark-snappy))

(in-package #:snappy-benchmark)

(defun benchmark-uncompress (bench octets)
  (let ((size (length octets)))
    ;; Report decompression bandwidth in terms of the size of the uncompressed data.
    (set-data-size bench size)
    (multiple-value-bind (compressed compressed-length)
        (compress octets 0 (length octets))
      (reset-timer bench)
      (benchmark-loop (bench)
        (snappy::raw-uncompress compressed 0 compressed-length octets 0 size)))))

(defun benchmark-compress (bench octets)
  (let ((size (length octets)))
    (set-data-size bench size)
    (let* ((max-compressed-length (maximum-compressed-length size))
           (compressed (make-octet-vector max-compressed-length)))
      (reset-timer bench)
      (benchmark-loop (bench)
        (snappy::raw-compress octets 0 size compressed 0 max-compressed-length)))))

(defun read-file (file-name)
  (with-open-file (stream file-name :element-type 'octet :if-does-not-exist :error)
    (let* ((octet-count (file-length stream))
           (data (make-octet-vector octet-count))
           (octets-read (read-sequence data stream)))
      (if (= octets-read octet-count)
        (values data t)
        (values (make-octet-vector 0) nil)))))

(defun expand (source-octets desired-size)
  (let ((expanded (make-octet-vector desired-size))
        (index 0))
    (loop while (plusp desired-size) do
      (let ((copy-size (min (length source-octets) desired-size)))
        (replace expanded source-octets
                 :start1 index
                 :end1 (+ index copy-size)
                 :start2 0
                 :end2 copy-size)
        (incf index copy-size)
        (decf desired-size copy-size)))
    expanded))

(defun words-benchmark (bench size operation)
  (let ((octets (expand (read-file "/usr/share/dict/words") size)))
    (ecase operation
      ((:compress) (benchmark-compress bench octets))
      ((:uncompress) (benchmark-uncompress bench octets)))))

(define-benchmark words-decode-1e1 "BenchmarkWordsDecode1e1" (bench)
  (words-benchmark bench 10 :uncompress))
(define-benchmark words-decode-1e2 "BenchmarkWordsDecode1e2" (bench)
  (words-benchmark bench 100 :uncompress))
(define-benchmark words-decode-1e3 "BenchmarkWordsDecode1e3" (bench)
  (words-benchmark bench 1000 :uncompress))
(define-benchmark words-decode-1e4 "BenchmarkWordsDecode1e4" (bench)
  (words-benchmark bench 10000 :uncompress))
(define-benchmark words-decode-1e5 "BenchmarkWordsDecode1e5" (bench)
  (words-benchmark bench 100000 :uncompress))
(define-benchmark words-decode-1e6 "BenchmarkWordsDecode1e6" (bench)
  (words-benchmark bench 1000000 :uncompress))
(define-benchmark words-encode-1e1 "BenchmarkWordsEncode1e1" (bench)
  (words-benchmark bench 10 :compress))
(define-benchmark words-encode-1e2 "BenchmarkWordsEncode1e2" (bench)
  (words-benchmark bench 100 :compress))
(define-benchmark words-encode-1e3 "BenchmarkWordsEncode1e3" (bench)
  (words-benchmark bench 1000 :compress))
(define-benchmark words-encode-1e4 "BenchmarkWordsEncode1e4" (bench)
  (words-benchmark bench 10000 :compress))
(define-benchmark words-encode-1e5 "BenchmarkWordsEncode1e5" (bench)
  (words-benchmark bench 100000 :compress))
(define-benchmark words-encode-1e6 "BenchmarkWordsEncode1e6" (bench)
  (words-benchmark bench 1000000 :compress))

(define-benchmark random-encode "BenchmarkRandomEncode" (bench)
  (let ((random (make-instance 'acm-random :seed 1))
        (octets (make-octet-vector (ash 1 20))))
    (dotimes (i (length octets))
      (setf (aref octets i) (next-uint8 random)))
    (benchmark-compress bench octets)))

(defun file-benchmark (bench file-name operation &optional limit)
  (let ((octets (read-data-file (concatenate 'string "bench/" file-name) limit)))
    (ecase operation
      ((:compress) (benchmark-compress bench octets))
      ((:uncompress) (benchmark-uncompress bench octets)))))

;; The naming convention follows that of the C++ and Go Snappy benchmarks.
(define-benchmark uflat0 "Benchmark_UFlat0" (bench)
  (file-benchmark bench "html" :uncompress))
(define-benchmark uflat1 "Benchmark_UFlat1" (bench)
  (file-benchmark bench "urls.10K" :uncompress))
(define-benchmark uflat2 "Benchmark_UFlat2" (bench)
  (file-benchmark bench "fireworks.jpeg" :uncompress))
(define-benchmark uflat3 "Benchmark_UFlat3" (bench)
  (file-benchmark bench "fireworks.jpeg" :uncompress 200))
(define-benchmark uflat4 "Benchmark_UFlat4" (bench)
  (file-benchmark bench "paper-100k.pdf" :uncompress))
(define-benchmark uflat5 "Benchmark_UFlat5" (bench)
  (file-benchmark bench "html_x_4" :uncompress))
(define-benchmark uflat6 "Benchmark_UFlat6" (bench)
  (file-benchmark bench "alice29.txt" :uncompress))
(define-benchmark uflat7 "Benchmark_UFlat7" (bench)
  (file-benchmark bench "asyoulik.txt" :uncompress))
(define-benchmark uflat8 "Benchmark_UFlat8" (bench)
  (file-benchmark bench "lcet10.txt" :uncompress))
(define-benchmark uflat9 "Benchmark_UFlat9" (bench)
  (file-benchmark bench "plrabn12.txt" :uncompress))
(define-benchmark uflat10 "Benchmark_UFlat10" (bench)
  (file-benchmark bench "geo.protodata" :uncompress))
(define-benchmark uflat11 "Benchmark_UFlat11" (bench)
  (file-benchmark bench "kppkn.gtb" :uncompress))

(define-benchmark zflat0 "Benchmark_ZFlat0" (bench)
  (file-benchmark bench "html" :compress))
(define-benchmark zflat1 "Benchmark_ZFlat1" (bench)
  (file-benchmark bench "urls.10K" :compress))
(define-benchmark zflat2 "Benchmark_ZFlat2" (bench)
  (file-benchmark bench "fireworks.jpeg" :compress))
(define-benchmark zflat3 "Benchmark_ZFlat3" (bench)
  (file-benchmark bench "fireworks.jpeg" :compress 200))
(define-benchmark zflat4 "Benchmark_ZFlat4" (bench)
  (file-benchmark bench "paper-100k.pdf" :compress))
(define-benchmark zflat5 "Benchmark_ZFlat5" (bench)
  (file-benchmark bench "html_x_4" :compress))
(define-benchmark zflat6 "Benchmark_ZFlat6" (bench)
  (file-benchmark bench "alice29.txt" :compress))
(define-benchmark zflat7 "Benchmark_ZFlat7" (bench)
  (file-benchmark bench "asyoulik.txt" :compress))
(define-benchmark zflat8 "Benchmark_ZFlat8" (bench)
  (file-benchmark bench "lcet10.txt" :compress))
(define-benchmark zflat9 "Benchmark_ZFlat9" (bench)
  (file-benchmark bench "plrabn12.txt" :compress))
(define-benchmark zflat10 "Benchmark_ZFlat10" (bench)
  (file-benchmark bench "geo.protodata" :compress))
(define-benchmark zflat11 "Benchmark_ZFlat11" (bench)
  (file-benchmark bench "kppkn.gtb" :compress))

(defun benchmark-snappy ()
  (run-benchmarks))
