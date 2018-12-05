#lang racket/base
;;
;; thrift - common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).


(require racket/contract)

(provide
 (all-defined-out))

;; ---------- Requirements

(require thrift/private/enumeration)

;; ---------- Implementation (Types)

(define-enumeration message-type 1
  (call
   reply
   exception
   one-way))

(define-enumeration exception-type 0
  (unknown
   unknown-method
   invalid-message-type
   wrong-method-name
   bad-sequence-id
   missing-result
   internal-error
   protocol-error
   invalid-transform
   invalid-protocol
   unsupported-client-type))

(define field-stop-value 0)

(define max-list-length 2147483647)
