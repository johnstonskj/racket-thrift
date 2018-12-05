#lang racket/base
;;
;; thrift - prototol.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

;; ---------- Requirements

(require rackunit
         ; ---------
         thrift/private/enumeration)

;; ---------- Test Fixtures

;; ---------- Internal procedures

;; ---------- Test Cases

(define-enumeration my-enum (A B C))

;(test-case
; "simple enum test"
; 
; (check-true (struct-constructor-procedure? my-enum))
; (check-true (struct-predicate-procedure? my-enum?))
; (check-false (my-enum? -1))
; (check-false (my-enum? 0))
; (check-true (my-enum? 1))
; (check-true (my-enum? 2))
; (check-true (my-enum? 3))
; (check-false (my-enum? 4))
; (check-true (my-enum? my-enum:A))
; (check-true (my-enum? my-enum:B))
; (check-true (my-enum? my-enum:C))
; (check-equal? (integer->my-enum 2) my-enum:C)
; (check-equal? my-enum/names '("my-enum-A" "my-enum-B" "my-enum-C")))

;(test-case
; "simple literal test"
;
; (define-syntax-literals kw-required (required optional))
;  
; (check-true (symbol? required))
; (check-true (symbol? optional))
; (check-true (kw-required? required))
; (check-true (kw-required? 'not-required)))

