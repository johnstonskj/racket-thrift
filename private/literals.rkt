#lang racket
;; thrift - idl/literals.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 (all-defined-out))

;; ---------- Requirements

(require (for-syntax racket/syntax
                     syntax/parse))

;; ---------- Implementation

(define-syntax (define-syntax-literals stx)
  (syntax-parse stx
    [(_ name:id (lit:id ...))
     (with-syntax ([set-id (format-id #'name "~a/set" #'name)]
                   [cls-id (format-id #'name "~a/class" #'name)]
                   [cls-description (format "~a literals" (syntax->datum #'name))]
                   [cls-var (format-id #'name "x:~a/class" #'name)]
                   [pred-id (format-id #'name "~a?" #'name)])
       #`(begin
           #,@(for/list ([literal (syntax->list #'(lit ...))])
                #`(define #,literal (quote #,literal)))
           (begin-for-syntax
             (define-literal-set set-id (lit ...))
             (define-syntax-class cls-id
               #:description cls-description
               #:literal-sets (set-id)
               (pattern (~or lit ...))))
           (define-syntax (pred-id stx)
             (syntax-parse stx
               [(_ cls-var) #'#t]
               [else #'#f]))))]))

