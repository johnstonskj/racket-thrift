#lang racket/base
;;
;; thrift - processor/common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 (all-defined-out))

;; ---------- Requirements

(require racket/contract
         thrift/protocol/common
         thrift/transport/common)

;; ---------- Implementation (Types)

(define transport-processor/c
  (-> input-transport? output-transport? boolean?))

(define protocol-processor/c
  (-> decoder? encoder? boolean?))
