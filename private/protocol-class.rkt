#lang racket/base
;;
;; thrift - private/protocol-class.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide

 (all-defined-out))

;; ---------- Requirements

(require
  (for-syntax
   racket/base
   racket/class
   racket/list
   racket/sequence
   racket/syntax)
  racket/class
  thrift/protocol/exn-common
  thrift/transport/common
  thrift/private/logging)

;; ---------- Implementation (Types)

                   
;; ---------- Implementation

(define-syntax (define-write stx)
  (syntax-case stx ()
    [(_ (id params ...) post-body ...)
     (and (identifier? #'id)
          (for/and ([an-id (syntax->list #'(params ...))])
            (identifier? an-id)))
     (with-syntax ([method-id (format-id #'id "write-~a" #'id)]
                   [method-log (format "~~a:write-~a ~~a" (syntax->datum #'id))])
       #`(define/pubment (method-id params ...)
           (define tport (get-field output-transport this))
           (unless (and (output-transport? tport) (not (send tport closed?)))
             (raise (transport-not-open-output (current-continuation-marks))))
           (log-thrift-debug method-log (send this name) (list params ...))
           (inner (void) method-id params ...)
           post-body ...))]))

(define-syntax (define-read stx)
  (syntax-case stx ()
    [(_ (id type-predicate? defvalue) post-body ...)
     (and (identifier? #'id)
          (identifier? #'type-predicate?))
     (with-syntax ([method-id (format-id #'id "read-~a" #'id)]
                   [method-log (format "~~a:read-~a returning ~~a" (syntax->datum #'id))])
       #`(define/pubment (method-id)
           (define tport (get-field input-transport this))
           (unless (and (input-transport? tport) (not (send tport closed?)))
             (raise (transport-not-open-input (current-continuation-marks))))
           (define value (inner defvalue method-id))
             (unless (type-predicate? value)
               (log-thrift-error "~a not-a ~a" value type-predicate?)
               (raise (invalid-value-type (current-continuation-marks) value)))
           (log-thrift-debug method-log (send this name) value)
           post-body ...
           value))]))

(define-syntax (define-wrapped stx)
  (syntax-case stx ()
    [(_ id ...)
     (for/and ([an-id (syntax->list #'(id ...))])
       (or (identifier? an-id)
           (and (list (syntax->datum #'an-id)))))
     #`(begin
         #,@(for/list ([an-id (syntax->list #'(id ...))])
             (cond
               [(identifier? an-id)
                (with-syntax ([method-id (format-id an-id "~a" an-id)])
                  #`(define/augment (method-id)
                      (send (get-field wrapped this) method-id)))]
               [(list? (syntax->datum an-id))
                (with-syntax ([method-id (format-id an-id "~a" (first (syntax->datum an-id)))]
                              [param-id (format-id an-id "~a" (second (syntax->datum an-id)))])
                  #`(define/augment (method-id param-id)
                      (send (get-field wrapped this) method-id param-id)))]
               )))]))
