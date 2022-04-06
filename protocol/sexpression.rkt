#lang racket/base
;;
;; thrift - protocol/sexpression.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide
 
 make-s-expression-protocol)

;; ---------- Requirements

(require racket/class
         thrift/protocol/common
         thrift/protocol/exn-common
         thrift/transport/common
         thrift/private/logging)

;; ---------- Internal types

(define *protocol*
  (protocol-id "s-expression" 0 1))

(struct s-value
  (index
   type
   value) #:prefab)

(struct protocol-header
  (id
   version
   message-header) #:prefab)

;; ---------- Implementation

(define s-expression-protocol%
  
  (class protocol%

    (super-new [identity *protocol*])

    (inherit-field input-transport
                   output-transport)
    
    (define inter-expression-space #\space)

    (define/augment (write-message-begin message)
      (write-value
       (protocol-header (protocol-id-string *protocol*)
                        (protocol-id-version *protocol*)
                        message)))

    (define/augment (write-struct-begin name)
      (write-value name))
    
    (define/augment (write-map-begin header)
      (write-value header))
    
    (define/augment (write-list-begin header)
      (write-value header))
    
    (define/augment (write-set-begin header)
      (write-value header))
    
    (define/augment (write-boolean value)
      (write-value value))
    
    (define/augment (write-byte value)
      (write-value value))
    
    (define/augment (write-bytes value)
      (write-value value))
    
    (define/augment (write-int16 value)
      (write-value value))
    
    (define/augment (write-int32 value)
      (write-value value))
    
    (define/augment (write-int64 value)
      (write-value value))
    
    (define/augment (write-double value)
      (write-value value))
    
    (define/augment (write-string value)
      (write-value value))

    (define/augment (read-message-begin)
      (define header (read-value protocol-header?))
      (when (not (equal? (protocol-header-id header) (protocol-id-string *protocol*)))
        (log-thrift-error "value ~s, invalid, expecting ~a"
                          (protocol-header-id header) 's-expression)
        (raise (invalid-protocol-id (current-continuation-marks) (protocol-header-id header))))
      (when (not (equal? (protocol-header-version header) 1))
        (log-thrift-error "value ~s, invalid, expecting ~a"
                          (protocol-header-version header) 1)
        (raise (invalid-protocol-version (current-continuation-marks) (protocol-header-version header))))
      (when (not (message-header? (protocol-header-message-header header)))
        (log-thrift-error "~s, invalid, expecting a message header"
                          (protocol-header-message-header header))
        (raise (decoding-error (current-continuation-marks) (protocol-header-message-header header))))
      (protocol-header-message-header header))

    (define/augment (read-struct-begin)
      (read-value string?))
    
    (define/augment (read-field-begin)
      (read-value field-header?))
    
    (define/augment (read-map-begin)
      (read-value map-header?))
    
    (define/augment (read-list-begin)
      (read-value list-header?))
    
    (define/augment (read-set-begin)
      (read-value list-header?))
    
    (define/augment (read-boolean)
      (read-value boolean?))
    
    (define/augment (read-byte)
      (read-value byte?))
    
    (define/augment (read-bytes)
      (read-value bytes?))
    
    (define/augment (read-int16)
      (read-value integer?))
    
    (define/augment (read-int32)
      (read-value integer?))
    
    (define/augment (read-int64)
      (read-value integer?))
    
    (define/augment (read-double)
      (read-value flonum?))
    
    (define/augment (read-string)
      (read-value string?))

    (define/private (write-value v)
      (send output-transport write v)
      (send output-transport write-byte inter-expression-space))

    (define/private (read-value type-predicate?)
      (log-thrift-debug "~a:read-value" (protocol-id-string *protocol*))
      (define v (send input-transport read))
      (cond
        [(type-predicate? v)
         (define spacer (send input-transport read-byte))
         (cond
           [(equal? spacer inter-expression-space)
            v]
           [else
            (log-thrift-error "unexpected spacer: ~a" spacer)
            (raise (decoding-error (current-continuation-marks) spacer))])]
        [else
         (log-thrift-error "~a not-a ~a" v type-predicate?)
         (raise (invalid-value-type (current-continuation-marks) v))]))))

(define (make-s-expression-protocol in-transport out-transport)
  (make-object s-expression-protocol% *protocol* in-transport out-transport))