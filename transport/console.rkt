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

(require racket/bool
         thrift/transport/common
         thrift/private/logging)

;; ---------- Implementation

(define (open-input-console-transport)
  (transport "console" 'stdin (current-input-port)))

(define (open-output-console-transport)
  (transport "console" 'stdout (current-output-port)))

