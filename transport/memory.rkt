#lang racket/base
;;
;; thrift - transport/memory.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 (contract-out
  
  [open-input-memory-transport
   (-> bytes? input-transport?)]

  [open-output-memory-transport
   (-> output-transport?)]

  [transport-output-bytes 
   (->  output-transport? bytes?)]))

;; ---------- Requirements

(require thrift/transport/common)

;; ---------- Implementation

(define (open-input-memory-transport bytes)
  (transport "in-memory" 'memory (open-input-bytes bytes)))

(define (open-output-memory-transport)
  (transport "in-memory" 'memory (open-output-bytes)))

(define (transport-output-bytes tport)
  (get-output-bytes (transport-port tport)))
