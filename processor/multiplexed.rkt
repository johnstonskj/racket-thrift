#lang racket/base
;;
;; thrift - processor/multiplexed.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide

 register-service

 deregister-service)

;; ---------- Requirements

(require racket/class
         thrift/protocol/multiplexed
         thrift/private/logging)

;; ---------- Implementation

(define service-registry (make-hash))

(define (register-service mux service-processor)
  (log-thrift-debug "multiplexed:register-service ~a ~a"
                    (get-field service-name mux) service-processor) 
  (unless (is-a? mux multiplexed-protocol%)
    (error "provided value is not a multiplexed protocol instance"))
  (hash-set! service-registry (get-field service-name mux) service-processor)
  (void))

(define (deregister-service mux)
  (log-thrift-debug "multiplexed:deregister-service ~a ~a" (get-field service-name mux)) 
  (unless (is-a? mux multiplexed-protocol%)
    (error "provided value is not a multiplexed protocol instance"))
  (hash-remove! service-registry (get-field service-name mux))
  (void))
