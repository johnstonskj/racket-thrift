#lang racket/base
;;
;; thrift - private/transport.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide

 no-op-encoder

 no-op-decoder

 flush-message-end

 read-plain-integer

 write-plain-integer)

;; ---------- Requirements

(require racket/bool
         thrift/transport/common
         thrift/private/logging)

;; ---------- Implementation

(define network-order-bytes #f)

(define (check-sequence cstate nstate)
  (cond
    [(and (equal? cstate 'message-begin)
          (not (equal? nstate 'struct-begin)))
     (error "invalid encoding state.")])
  #t)

(define (no-op-encoder name)
  (log-thrift-debug (symbol->string name)))

(define (flush-message-end transport)
  (flush-transport transport))

(define (no-op-decoder name)
  (log-thrift-debug (symbol->string name)))

(define (read-plain-integer in width-in-bytes [signed #t])
  (unless (input-transport? in) (error "transport must be open for input"))
  (define bs (transport-read-bytes in width-in-bytes))
  (integer-bytes->integer bs signed network-order-bytes 0 width-in-bytes))

(define (write-plain-integer out num width-in-bytes [signed #t])
  (unless (output-transport? out) (error "transport must be open for output"))
  (define bs (integer->integer-bytes num width-in-bytes signed network-order-bytes))
  (transport-write-bytes out bs))
  

