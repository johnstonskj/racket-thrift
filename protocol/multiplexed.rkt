#lang racket/base
;;
;; thrift - protocol/multiplexed.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

;; https://github.com/apache/thrift/blob/master/doc/specs/thrift-binary-protocol.md

(require racket/contract)

(provide

 (contract-out
  
  [make-multiplexed-encoder
   (-> encoder? string? (or/c encoder? #f))]
  
  [make-multiplexed-decoder
   (-> decoder? string? (or/c decoder? #f))]

  [register-service
   (-> decoder? string? protocol-processor/c void?)]

  [deregister-service
   (-> decoder? string? void?)])

 (struct-out mux-message-header))

;; ---------- Requirements

(require racket/flonum
         racket/list
         racket/string
         thrift/protocol/common
         thrift/protocol/exn-common
         thrift/processor/common
         thrift/private/logging)

;; ---------- Implementation

(define *protocol*
  (protocol-id "multiplexed" 0 1))

(define service-name-separator (make-parameter ":"))

(struct mux-message-header message-header
  (service-name) #:transparent)

(define (make-multiplexed-encoder wrapped service-name)
  (encoder
   (protocol-id-string *protocol*)
   (λ (header) (encode-mux-message-begin header wrapped service-name))
   (λ () ((encoder-message-end wrapped)))
   (λ (name) ((encoder-struct-begin wrapped name)))
   (λ () ((encoder-struct-end wrapped)))
   (λ (header) ((encoder-field-begin wrapped) header))
   (λ () ((encoder-field-end wrapped)))
   (λ (header) ((encoder-map-begin wrapped) header))
   (λ () ((encoder-map-end wrapped)))
   (λ (header) ((encoder-list-begin wrapped) header))
   (λ () ((encoder-list-end wrapped)))
   (λ (header) ((encoder-set-begin wrapped) header))
   (λ () ((encoder-set-end wrapped)))
   (λ () ((encoder-boolean wrapped)))
   (λ () ((encoder-byte wrapped)))
   (λ () ((encoder-bytes wrapped)))
   (λ () ((encoder-int16 wrapped)))
   (λ () ((encoder-int32 wrapped)))
   (λ () ((encoder-int64 wrapped)))
   (λ () ((encoder-double wrapped)))
   (λ () ((encoder-string wrapped)))))

(struct mux-decoder decoder
  (service-registry))

(define (make-multiplexed-decoder wrapped service-name)
  (mux-decoder
   (protocol-id-string *protocol*)
   (λ () (decode-mux-message-begin wrapped))
   (λ () ((decoder-message-end wrapped)))
   (λ () ((decoder-struct-begin wrapped)))
   (λ () ((decoder-struct-end wrapped)))
   (λ () ((decoder-field-begin wrapped)))
   (λ () ((decoder-field-end wrapped)))
   (λ () ((decoder-map-begin wrapped)))
   (λ () ((decoder-map-end wrapped)))
   (λ () ((decoder-list-begin wrapped)))
   (λ () ((decoder-list-end wrapped)))
   (λ () ((decoder-set-begin wrapped)))
   (λ () ((decoder-set-end wrapped)))
   (λ () ((decoder-boolean wrapped)))
   (λ () ((decoder-byte wrapped)))
   (λ () ((decoder-bytes wrapped)))
   (λ () ((decoder-int16 wrapped)))
   (λ () ((decoder-int32 wrapped)))
   (λ () ((decoder-int64 wrapped)))
   (λ () ((decoder-double wrapped)))
   (λ () ((decoder-string wrapped)))
   (make-hash)))

(define (register-service mux service-name service-processor)
  (log-thrift-debug "multiplexed:register-service ~a ~a" service-name service-processor) 
  (unless (mux-decoder? mux)
    (error "provided value is not a multiplexed decoder"))
  (hash-set! (mux-decoder-service-registry mux) service-name service-processor)
  (void))

(define (deregister-service mux service-name)
  (log-thrift-debug "multiplexed:deregister-service ~a ~a" service-name) 
  (unless (mux-decoder? mux)
    (error "provided value is not a multiplexed decoder"))
  (hash-remove! (mux-decoder-service-registry mux) service-name)
  (void))

;; ---------- Internal procedures

(define (encode-mux-message-begin msg-header wrapped  service-name)
  (define new-name (format "~a~a~a"
                           service-name
                           (service-name-separator)
                           (message-header-name msg-header)))
  ((decoder-message-end wrapped)
   (struct-copy message-header
                msg-header
                [name new-name])))

(define (decode-mux-message-begin wrapped)
  (define msg-header ((decoder-message-end wrapped)))
  (define names (string-split (message-header-name msg-header) (service-name-separator)))
  (define-values (service method)
    (cond
      [(= (length names) 1)
       (values "" (first names))]
      [(= (length names) 2)
       (values (first names) (second names))]
      [else (raise wrong-method-name)]))
  (mux-message-header
   method
   (message-header-type msg-header)
   (message-header-sequence-id msg-header)
   service))
   