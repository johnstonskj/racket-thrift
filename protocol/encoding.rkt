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
   (-> any/c exn:thrift? (or/c message-header? #f) void?)]))

;; ---------- Requirements

(require racket/bool
         thrift/common
         thrift/protocol/common
         thrift/exn-common)

;; ---------- Implementation

(define (encode-exn protocol exn reply-to)
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
  
  (send protocol write-message-begin header)

  (send protocol write-struct-begin "exception")

  (send protocol write-field-begin (field-header "message" type-string 1))
  (send protocol write-string (exn-message exn))
  (send protocol write-field-end)

  (send protocol write-field-begin (field-header "type" type-int32 2))
  (send protocol write-int32 (exn:thrift-type exn))
  (send protocol write-field-end)

  (send protocol write-field-stop)
  (send protocol write-struct-end)
  
  (send protocol write-message-end))

;(module+ test
;  (require rackunit
;           thrift/transport/memory
;           thrift/protocol/exn-common
;           thrift/protocol/sexpression
;           thrift/private/bytedebug)
;
;  (define tout (open-output-memory-transport))
;  (define pout (make-sexpression-encoder tout))
;
;  (define exn (wrong-method-name (current-continuation-marks) "hello?"))
;  
;  (encode-exn pout
;              exn
;              (message-header "hello?" message-type-call 11))
;
;  (close-transport tout)
;  (check-equal? (transport-output-bytes tout)
;                (bytes-append #"#s(protocol-header \"s-expression\" 1 #s(message-header \"hello?\""
;                              #" 3 11)) #s(field-header \"message\" 11 1) \"missing, or wrong,"
;                              #" method name: hello?\" #s(field-header \"type\" 8 2) 3 ")))