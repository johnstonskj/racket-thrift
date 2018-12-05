#lang racket/base
;;
;; thrift - idl/language.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 
 define-thrift-namespace

 define-thrift-enum

 define-thrift-struct)

;; ---------- Requirements

(require thrift/private/literals
         (for-syntax
          racket/base
          racket/sequence
          racket/syntax
          syntax/parse))

;; ---------- Implementation

(define-syntax (define-thrift-namespace stx)
  (syntax-case stx ()
    [(_ ns)
     (identifier? #'ns)
     #'(define thrift-namespace (quote ns))]))

(define-syntax (define-thrift-enum stx)
  (syntax-case stx ()
    [(_ id (enum-id ...))
     ;; no starting integer specified, use the default of one.
     #`(define-thrift-enum id 1 (enum-id ...))]
    ;; starting integer specified, just increase by one for each identifier.

    [(_ id ([enum-id enum-val] ...))
     ;; no starting integer specified, use the default of one.
     #`(define-thrift-enum id 1 ([enum-id enum-val] ...))]

    [(_ id start (enum-id ...))
     (and (identifier? #'id)
          (for/and ([an-id (syntax->list #'(enum-id ...))])
            (identifier? an-id)))
     #`(begin
         (struct id (v))
         #,@(let* ([first-integer (syntax->datum #'start)]
                   [enum-list (syntax->list #'(enum-id ...))]
                   [integers (sequence->list (in-range first-integer (add1 (length enum-list))))])
              ;; define each enumeration value
              (for/list ([x enum-list]
                         [n integers])
                (with-syntax ([an-enum-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define an-enum-id (id ix))))))]
                
    [(_ id start ([enum-id enum-val] ...))
     ;; TODO: this should be the workhorse version
     (error "unsupported format")]))

(define-syntax-literals kw-required (required optional))

(define-syntax-literals kw-container (list-of set-of map-of none))

(define-syntax (define-thrift-struct stx)
  (syntax-parse stx
    [(_ struct-id:id
        ((~seq [index:nat name:id . rest] ...)))

     (with-syntax ([decode-name (format-id #'struct-id "~a/decode" #'struct-id)]
                   [field-list 
                    (for/list ([a-name (syntax->list #'(name ...))])
                      (format-id #'struct-id "~a" a-name))])
       #'(begin
           (struct struct-id field-list)
           (define (decode-name decoder) (void))))]))

