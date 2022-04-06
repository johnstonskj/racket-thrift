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

(require racket/class
         thrift/transport/common)

;; ---------- Implementation

(define *transport-name* "in-memory")

(define (open-input-memory-transport bytes)
  (make-object transport% *transport-name* 'memory (open-input-bytes bytes)))

(define (open-output-memory-transport)
  (make-object transport% *transport-name* 'memory (open-output-bytes)))

(define (transport-output-bytes tport)
  (get-output-bytes (get-field port tport)))
