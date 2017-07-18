;;
;; Helper functions to read integers from a stream.
;; Stream representation of integers is big endian.
;; Todo: Performance optimizations
;;


(in-package :clws.binary-types)

;; a read-byte wrapper with logging for debugging purposes
(defun read-uint-8 (stream)
  "Read a byte"
  (let ((b (read-byte stream)))
    b))

(defun read-uint-16 (stream)
  "Read a 16 bit integer"
  (+ (* (read-uint-8 stream) 256)
     (read-uint-8 stream)))

(defun read-uint-32 (stream)
  "Read a 32 bit integer"
  (+ (* (read-uint-8 stream) 16777216)
     (* (read-uint-8 stream) 65536)
     (* (read-uint-8 stream) 256)
     (read-uint-8 stream)))

(defun read-uint-64 (stream)
  "Read a 64 bit integer"
  (+ (* (read-uint-32 stream) (expt 256 4))
     (read-uint-32 stream)))

(defun write-uint-16 (i stream)
  "Write a 16 bit integer"
  (multiple-value-bind (hi lo) 
      (floor i 256)
    (write-byte hi stream)
    (write-byte lo stream)))

(defun write-uint-32 (i stream)
  "Write a 32 bit integer"
  (multiple-value-bind  (hi lo)
      (floor i 65536)
    (write-uint-16 hi stream)
    (write-uint-16 lo stream)))

(defun write-uint-64 (i stream)
  "Write a 64 bit integer"
  (multiple-value-bind (hi lo)
      (floor i (* 65536 65536))
    (write-uint-32 hi stream)
    (write-uint-32 lo stream)))
