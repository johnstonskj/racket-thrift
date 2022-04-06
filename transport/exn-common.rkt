#lang racket/base
;;
;; thrift - transport/exn-common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).


(provide
 (all-defined-out))

(require thrift/exn-common)

(define (file-not-exists c v)
  (exn:thrift:transport (string-append "file does not exist: " v) c 201))

(define (permission-error-read c)
  (exn:thrift:transport "read permission denied" c 202))

(define (permission-error-write c)
  (exn:thrift:transport "write permission denied" c 203))

(define (not-open-for-input c)
  (exn:thrift:transport "port not open for input" c 204))

(define (not-open-for-output c)
  (exn:thrift:transport "port not open for output" c 205))
