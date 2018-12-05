#lang racket/base
;;
;; thrift - protocol/exn-common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require thrift/exn-common)

(provide
 (all-defined-out))

(define (transport-not-open-input c)
  (exn:thrift:protocol "transport must be open for input" c 101))

(define (transport-not-open-output c)
  (exn:thrift:protocol "transport must be open for output" c 101))

(define (invalid-message-type c v)
  (exn:thrift:protocol (format "missing, or invalid, message type: ~a" v) c 2))

(define (wrong-method-name c v)
  (exn:thrift:protocol (format "missing, or wrong, method name: ~a" v) c 3))

(define (encoding-error c n v)
  (exn:thrift:protocol (format "invalid value/type when encoding ~a: ~a" n v) c 7))

(define (decoding-error c n v)
  (exn:thrift:protocol (format "invalid value/type when decoding ~a: ~a" n v) c 7))

(define (number-too-large c w v)
  (exn:thrift:protocol (format "number greater than expected size (~a) in bits: ~a" w v) c 7))

(define (number-negative c v)
  (exn:thrift:protocol (format "cannot 7bit encode negative numbers: ~a" v) c 7))

(define (invalid-field-id-zero c)
  (exn:thrift:protocol "field id may not be zero" c 7))

(define (invalid-field-type c v)
  (exn:thrift:protocol (format "invalid field type: ~a" v) c 7))

(define (invalid-value-type c v)
  (exn:thrift:protocol (format "unexpected type for value: ~a" v) c 7))

(define (invalid-protocol-id c v)
  (exn:thrift:protocol (format "missing, or invalid, protocol id ~a" v) c 9))

(define (invalid-protocol-version c v)
  (exn:thrift:protocol (format "missing, or invalid, protocol version: ~a" v) c 9))