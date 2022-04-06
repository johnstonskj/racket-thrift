#lang racket/base
;;
;; thrift - protocol/decoding.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 (all-defined-out))

;; ---------- Requirements

(require racket/class
         racket/list
         racket/struct
         thrift
         thrift/private/logging)

;; ---------- Implementation

(define (type-bool/decode decoder)
  (send decoder read-boolean))

(define (type-bool/decode-list decoder)
  (decode-a-list decoder type-bool/decode))

(define (type-byte/decode decoder)
  (send decoder read-byte))

(define (type-byte/decode-list decoder)
  (decode-a-list decoder type-byte/decode))

(define (type-binary/decode decoder)
  (send decoder read-bytes))

(define (type-binary/decode-list decoder)
  (decode-a-list decoder type-binary/decode))

(define (type-int16/decode decoder)
  (send decoder read-int16))

(define (type-int16/decode-list decoder)
  (decode-a-list decoder type-int16/decode))

(define (type-int32/decode decoder)
  (send decoder read-int32))

(define (type-int32/decode-list decoder)
  (decode-a-list decoder type-int32/decode))

(define (type-int64/decode decoder)
  (send decoder read-int64))

(define (type-int64/decode-list decoder)
  (decode-a-list decoder type-int64/decode))

(define (type-double/decode decoder)
  (send decoder read-double))

(define (type-double/decode-list decoder)
  (decode-a-list decoder type-double/decode))

(define (type-string/decode decoder)
  (send decoder read-string))

(define (type-string/decode-list decoder)
  (decode-a-list decoder type-string/decode))

(define (decode-a-list decoder element-decoder)
  (log-thrift-debug "decoding a list of ~a" (object-name element-decoder))
  (define header (send decoder read-list-begin))
  (define the-list
    (for/list ([element (range (list-header-length header))])
      (cond
        [(= (procedure-arity element-decoder) 0)
         (element-decoder)]
        [(= (procedure-arity element-decoder) 1)
         (element-decoder decoder)]
        [else
         (error "invalid decoder function: " (object-name element-decoder))])))
  (send decoder read-list-end)
  the-list)

(define (decode-a-union decoder constructor struct-schema)
  ;; identical to decoding a structure, and the field index tells us
  ;; which union element is encoded. We simply check after the fact that
  ;; only one element has been decoded.
  (log-thrift-debug "decoding a union of ~a" (object-name constructor))
  (define the-struct (decode-a-struct decoder constructor struct-schema))
  (when
      (> (length (filter (λ (e) (not (equal? 'no-value e))) (struct->list the-struct)) 1))
    (error "union may only have at most one set field ~a" (object-name constructor)))
  the-struct)

; schema : hash/c exact-nonnegative-integer? thrift-field
(define (decode-a-struct decoder constructor struct-schema)
  (log-thrift-debug "decoding a structure of ~a" (object-name constructor))
  (send decoder read-struct-begin)
  (define result (make-vector (hash-count struct-schema) 'no-value))
  
  (let next-field ([field (send decoder read-field-begin)])
    (cond
      [(= (field-header-type field) field-stop-value)
       (send decoder read-field-end)]
      [else
       ;; TODO: handle booleans separately
       (define schema (hash-ref struct-schema (field-header-id field)))
       (define decode-func (thrift-field-major-type schema))
;       (log-thrift-debug "using decoder function ~a" decode-func)
       (define value
         (cond
           [(= (procedure-arity decode-func) 0)
            (decode-func)]
           [(= (procedure-arity decode-func) 1)
            (decode-func decoder)]
           [else
            (error "invalid decoder function: " (object-name decode-func))]))
       (log-thrift-debug "<< field value for ~a (~a): ~a"
                         (field-header-id field)
                         (thrift-field-name schema)
                         value)
       (vector-set! result (thrift-field-position schema) value)
       (send decoder read-field-end)
       (next-field (send decoder read-field-begin))]))

  (for ([(id schema) struct-schema])
    (when (and
           (equal? (thrift-field-required schema) 'required)
           (equal? (vector-ref result (thrift-field-position schema)) 'no-value))
      (error (format "<< field id ~a (~a) required for structure ~a "
                     id
                     (thrift-field-name schema)
                     (object-name constructor)))))
  
  (send decoder read-struct-end)
  (apply constructor (vector->list result)))

;; ---------- Internal procedures

;; ---------- Internal tests
