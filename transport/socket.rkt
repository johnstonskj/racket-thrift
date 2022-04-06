#lang racket/base
;;
;; thrift - transport/socket.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 (contract-out
  
  [open-socket-transport
   (->* ((or/c 'tcp 'udp 'unix) string?)
        (exact-positive-integer?)
        (values input-transport? output-transport?))]))

;; ---------- Requirements

(require racket/bool
         racket/class
         racket/tcp
         racket/udp
         racket/unix-socket
         thrift/transport/common)

;; ---------- Implementation

(define (open-socket-transport protocol hostname [port-no #f])
  (define-values (inp outp)
    (cond
      [(symbol=? protocol 'tcp)
       (tcp-connect hostname port-no)]
      [(symbol=? protocol 'udp)
       (udp-bind! (udp-open-socket hostname port-no) hostname port-no)]
      [(symbol=? protocol 'unix)
       (unless unix-socket-available?
         (error "AF_UNIX sockets not available"))
       (unix-socket-connect hostname)]
      [else (error "unknown protocol: " protocol)]))
  (define source (if (false? port-no) hostname (format "~a:~a" hostname port-no)))
  (values
   (make-object transport% "in-socket" source inp)
   (make-object transport% "out-socket" source inp)))
