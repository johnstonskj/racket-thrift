#lang racket/base
;;
;; thrift - protocol/binary.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

;; https://github.com/apache/thrift/blob/master/doc/specs/thrift-binary-protocol.md

(require racket/contract)

(provide

 (contract-out
  
  [make-binary-protocol
   (-> (or/c input-transport? #f) (or/c output-transport? #f) (or/c protocol? #f))]))

;; ---------- Requirements

(require racket/bool
         racket/class
         racket/flonum
         thrift/idl/common
         thrift/protocol/common
         thrift/protocol/exn-common
         thrift/transport/common
         thrift/private/protocol
         thrift/private/logging)

;; ---------- Internal types/values

(define *protocol*
  (protocol-id "binary" 0 #b1000000000000001))

(define unnamed "")

;; ---------- Implementation

(define binary-protocol%
  
  (class protocol%

    (super-new [identity *protocol*])

    (inherit-field input-transport
                   output-transport)
    
    (define/augment (write-message-begin header)
      (write-plain-integer output-transport (protocol-id-version *protocol*) 2 #f)
      (send output-transport write-byte 0)
      (send output-transport
            write-byte
            (bitwise-and (message-header-type header) #b111))
      (send this write-string (message-header-name header))
      (write-plain-integer output-transport (message-header-sequence-id header) 4))

    (define/augment (write-field-begin header)
      (cond
        [(equal? (field-header-type header) type-stop)
         (send this write-byte type-stop)]
        [else
         (send this write-byte (field-header-type header))
         (write-plain-integer output-transport (field-header-id header) 2)]))

    (define/augment (write-map-begin header)
      (send this write-byte (map-header-key-type header))
      (send this write-byte (map-header-element-type header))
      (write-plain-integer output-transport (map-header-length header) 4))

    (define/augment (write-list-begin header)
      (send this write-byte (list-header-element-type header))
      (write-plain-integer output-transport (list-header-length header) 4))

    (define/augment (write-set-begin header)
      (send this write-list-begin header))
    
    (define/augment (write-boolean v)
      (if (false? v)
          (send this write-byte 0)
          (send this write-byte 1)))

    (define/augment (write-byte v)
      (send output-transport write-byte v))

    (define/augment (write-bytes v)
      (write-plain-integer output-transport (bytes-length v) 4)
      (send output-transport write-bytes v))

    (define/augment (write-int16 v)
      (write-plain-integer output-transport v 2))
    
    (define/augment (write-int32 v)
      (write-plain-integer output-transport v 4))
    
    (define/augment (write-int64 v)
      (write-plain-integer output-transport v 8))
    
    (define/augment (write-double tport v)
      (write-plain-integer output-transport (fl->exact-integer v) 8))
    
    (define/augment (write-string tport str)
      (send output-transport write-bytes (string->bytes/utf-8 str)))
    
    (define/augment (read-message-begin)
      (define msg-version (read-plain-integer input-transport 2 #f))
      (unless (= msg-version (protocol-id-version *protocol*))
        (raise (invalid-protocol-version (current-continuation-marks) msg-version)))
      
      (send input-transport read-byte) ;; ignored
      
      (define msg-type-byte (send input-transport read-byte))
      ;; TODO: check top 5 bytes are 0
      (define msg-type (bitwise-and msg-type-byte #b111))
      
      (define msg-method-name (send input-transport read-string))
      (when (= (string-length msg-method-name) 0)
        (raise (wrong-method-name (current-continuation-marks) msg-method-name)))
      
      (define msg-sequence-id (read-plain-integer input-transport 4))
      (log-thrift-debug "message name ~a, type ~s, sequence ~a"
                        msg-method-name msg-type msg-sequence-id)
      (message-header msg-method-name msg-type msg-sequence-id))
    
    (define/augment (read-field-begin)
      (define field-type (send input-transport read-byte))
      (cond
        [(= field-type type-stop)
         (log-thrift-debug "<< (field-stop)")
         (field-header unnamed type-stop type-stop)]
        [else
         (define field-id (read-plain-integer input-transport 2))
         (log-thrift-debug "<< structure field id ~a type ~a (~s)"
                           field-id field-type (integer->type field-type))
         (field-header unnamed field-type field-id)]))

    (define/augment (read-map-begin)
      (define key-type (send input-transport read-byte))
      (define element-type (send input-transport read-byte))
      (define size (read-plain-integer input-transport 4))
      (map-header key-type element-type size))

    (define/augment (read-list-begin)
      (define element-type (send input-transport read-byte))
      (define size (read-plain-integer input-transport 4))
      (log-thrift-debug "<< reading list, ~a elements, of type ~s"
                        size (integer->type element-type))
      (list-header element-type size))

    (define/augment (read-set-begin)
      (send this read-list-begin))

    (define/augment (read-boolean)
      (if (= (send input-transport read-byte) 0) #f #t))
    
    (define/augment (read-byte)
      (if (= (send input-transport read-byte) 0) #f #t))
    
    (define/augment (read-bytes)
      (define byte-length (read-plain-integer input-transport 4))
      (send input-transport read-bytes byte-length))

    (define/augment (read-int16)
      (read-plain-integer input-transport 2))
    
    (define/augment (read-int32)
      (read-plain-integer input-transport 4))
    
    (define/augment (read-int64)
      (read-plain-integer input-transport 8))
    
    (define/augment (read-double)
      (->fl (read-plain-integer input-transport 8)))
    
    (define/augment (read-string)
      (bytes->string/utf-8 (send this read-bytes)))))

(define (make-binary-protocol in-transport out-transport)
  (make-object binary-protocol% *protocol* in-transport out-transport))