#lang racket/base
;;
;; thrift - Test Documentation Coverage.
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 (contract-out

  [test-doc-coverage
   (-> (listof string?) any)]))

;; ---------- Requirements

(require racket/string
         rackunit
         rackunit/docs-complete)

;; ---------- Test Utilities

(define (test-doc-coverage module-list)
  (for ([module module-list])
    (test-case
     (format "test for documentation in ~a" module)
     (let ([s (open-output-string)])
       (parameterize ([current-error-port s])
         (check-docs module))
       (define out (get-output-string s))
       (when (non-empty-string? out)
         (displayln out))
       (check-eq? (string-length out) 0)))))

(test-doc-coverage
 '(thrift/idl/common
   thrift/idl/language
   thrift/idl/generator
   
   thrift/processor/common
   
   thrift/protocol/binary
   thrift/protocol/common
   thrift/protocol/compact
   thrift/protocol/json
   thrift/protocol/multiplexed
   thrift/protocol/sexpression
   thrift/protocol/encoding
   thrift/protocol/decoding
   
   thrift/transport/buffered
   thrift/transport/common
   thrift/transport/console
   thrift/transport/file))
   ; not yet thrift/transport/serializer

