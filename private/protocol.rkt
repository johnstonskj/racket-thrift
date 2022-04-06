#lang racket/base
;;
;; thrift - private/protocol.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide

 read-plain-integer

 write-plain-integer)

;; ---------- Requirements

(require racket/bool
         racket/class
         thrift/transport/common
         thrift/private/logging)

;; ---------- Implementation (Types)

                   
;; ---------- Implementation

(define network-order-bytes #f)

(define (read-plain-integer in width-in-bytes [signed #t])
  (unless (input-transport? in) (error "transport must be open for input"))
  (define bs (send in read-bytes width-in-bytes))
  (integer-bytes->integer bs signed network-order-bytes 0 width-in-bytes))

(define (write-plain-integer out num width-in-bytes [signed #t])
  (unless (output-transport? out) (error "transport must be open for output"))
  (define bs (integer->integer-bytes num width-in-bytes signed network-order-bytes))
  (send out write-bytes out bs))
