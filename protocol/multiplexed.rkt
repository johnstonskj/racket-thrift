#lang racket/base
;;
;; thrift - protocol/multiplexed.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 multiplexed-protocol%

 make-multiplexed-protocol
 
 (struct-out mux-message-header))

;; ---------- Requirements

(require racket/class
         racket/list
         racket/string
         thrift/protocol/common
         thrift/protocol/exn-common
         thrift/protocol/wrapper)

;; ---------- Implementation (Types)

(struct mux-message-header message-header
  (service-name) #:transparent)

;; ---------- Implementation

(define *protocol*
  (protocol-id "multiplexed" 0 1))

(define service-name-separator (make-parameter ":"))

(define multiplexed-protocol%
  
  (class protocol-wrapper%

    (super-new [identity *protocol*]
               [input-transport 'unused]
               [output-transport 'unused])

    (init-field service-name)

    (inherit-field wrapped)

    (define/augment (write-message-begin message)
      (define new-name (format "~a~a~a"
                               service-name
                               (service-name-separator)
                               (message-header-name message)))
      (send wrapped
            write-message-begin
            (struct-copy message-header
                         message
                         [name new-name])))

    (define/augment (read-message-begin)
      (define msg-header (send wrapped read-message-begin))
      (define names (string-split (message-header-name msg-header) (service-name-separator)))
      (define-values (service method)
        (cond
          [(= (length names) 1)
           (values "" (first names))]
          [(= (length names) 2)
           (values (first names) (second names))]
          [else (raise wrong-method-name)]))
      (mux-message-header
       method
       (message-header-type msg-header)
       (message-header-sequence-id msg-header)
       service))))

(define (make-multiplexed-protocol wrapped service-name)
  (make-object multiplexed-protocol% [wrapped wrapped] [service-name service-name]))