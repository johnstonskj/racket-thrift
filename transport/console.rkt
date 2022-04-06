#lang racket/base
;;
;; thrift - transport/console.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 (contract-out
  
  [open-input-console-transport
   (-> transport?)]

  [open-output-console-transport
   (-> transport?)]))

;; ---------- Requirements

(require racket/class
         thrift/transport/common)

;; ---------- Implementation

(define *transport-name* "console")

(define (open-input-console-transport)
  (make-object transport% *transport-name* 'stdin (current-input-port)))

(define (open-output-console-transport)
  (make-object transport% *transport-name* 'stdout (current-output-port)))

