#lang racket/base
;;
;; thrift - idl/enumeration.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 (all-defined-out))

;; ---------- Requirements

(require (for-syntax
          racket/base
          racket/list
          racket/match
          racket/sequence
          racket/syntax
          syntax/parse))

;; ---------- Implementation

(begin-for-syntax
  (define-syntax-class name-value
    #:description "name/value pair"
    #:attributes (name value)
    (pattern (name:id value:nat))))

(define-syntax (define-struct-enumeration stx)
  (define-values (enum-id name-values)
    (syntax-parse stx
      #:context 'enumeration-specification
      [(_ enum-id:id (~optional start:nat #:defaults ([start #'0])) ((~seq idents:id ...)))
       (define names (syntax->list #'(idents ...)))
       (define start-num (syntax->datum #'start))
       (values (syntax->datum #'enum-id)
               (for/list ([name names] [v (range start-num (+ (length names) start-num))])
                 (cons (syntax->datum name) v)))]
      [(_ enum-id:id (~optional start:nat #:defaults ([start #'0])) ((~seq idents:name-value ...)))
       (values (syntax->datum #'enum-id)
               (for/list ([name (syntax->list #'(idents.name ...))]
                          [v (syntax->list #'(idents.value ...))])
                 (cons (syntax->datum name) (syntax->datum v))))]))
  (with-syntax ([enum-name (format-id stx "~a-n" enum-id)]
                [enum-value (format-id stx "~a-v" enum-id)]
                [enum-symbol (format-id stx "~a->symbol" enum-id)]
                [enum-integer (format-id stx "~a->integer" enum-id)]
                [integer-enum (format-id stx "integer->~a" enum-id)])
    #`(begin
        (require racket/match racket/struct)
        (struct #,enum-id (n v) #:transparent)
        #,@(for/list ([vs name-values])
             (with-syntax ([value-name (format-id stx "~a:~a" enum-id (car vs))])
               #`(define value-name (#,enum-id 'value-name #,(cdr vs)))))
        (define (enum-symbol e) (car (struct->list e)))
        (define (enum-integer e) (cadr (struct->list  e)))
        (define (integer-enum n)
          (match n
            #,@(for/list ([vs name-values])
                 (with-syntax ([value-name (format-id #'id "~a:~a" enum-id (car vs))])
                   #`[#,(cdr vs) value-name]))
            [else (error "unknown value for enum ~a: " n)])))))
