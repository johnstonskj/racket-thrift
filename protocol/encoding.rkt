#lang racket/base
;;
;; thrift - protocol/encoding.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide
 
 (contract-out

  [encode-exn
   (-> encoder? exn:thrift? (or/c message-header? #f) void?)]))

;; ---------- Requirements

(require racket/bool
         thrift
         thrift/exn-common)

;; ---------- Implementation

(define (encode-exn pout exn reply-to)
  ;;
  ;; exception TApplicationException {
  ;;   1: string message,
  ;;   2: i32 type
  ;; }
  ;; Taken from https://github.com/apache/thrift/blob/master/doc/specs/thrift-rpc.md
  ;;
  (define header
    (cond
      [(false? reply-to)
       (message-header "" message-type-exception 0)]
      [else
       (message-header
        (message-header-name reply-to)
        message-type-exception
        (message-header-sequence-id reply-to))]))
  ((encoder-message-begin pout) header)

  ((encoder-struct-begin pout) "exception")

  ((encoder-field-begin pout) (field-header "message" type-string 1))
  ((encoder-string pout) (exn-message exn))
  ((encoder-field-end pout))

  ((encoder-field-begin pout) (field-header "type" type-int32 2))
  ((encoder-int32 pout) (exn:thrift-type exn))
  ((encoder-field-end pout))

  ((encoder-field-stop pout))
  ((encoder-struct-end pout))
  
  ((encoder-message-end pout)))

(module+ test
  (require rackunit
           thrift/transport/memory
           thrift/protocol/exn-common
           thrift/protocol/sexpression
           thrift/private/bytedebug)

  (define tout (open-output-memory-transport))
  (define pout (make-sexpression-encoder tout))

  (define exn (wrong-method-name (current-continuation-marks) "hello?"))
  
  (encode-exn pout
              exn
              (message-header "hello?" message-type-call 11))

  (close-transport tout)
  (check-equal? (transport-output-bytes tout)
                (bytes-append #"#s(protocol-header \"s-expression\" 1 #s(message-header \"hello?\""
                              #" 3 11)) #s(field-header \"message\" 11 1) \"missing, or wrong,"
                              #" method name: hello?\" #s(field-header \"type\" 8 2) 3 ")))