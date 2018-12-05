#lang racket/base
;;
;; thrift - transport.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

;; ---------- Requirements

(require racket/bool
         racket/list
         rackunit
         ; ---------
         thrift
         thrift/protocol/binary
         thrift/protocol/compact
         thrift/protocol/json
         thrift/protocol/sexpression
         thrift/transport/buffered
         thrift/transport/memory
         thrift/private/bytedebug)

;; ---------- Test Fixtures

(define binary-data
  (bytes-append #"Racket is a general-purpose programming language as well as "
                #"the world's first ecosystem for language-oriented programming"))

;; ---------- Internal procedures

(define (test-write-then-read encoder decoder wrapper-out wrapper-in)
  (define traw-out (open-output-memory-transport))
  (define tout (if (false? wrapper-out) traw-out (wrapper-out traw-out)))
  (define pout (encoder tout))

  ((encoder-message-begin pout) (message-header "test" message-type-call 42))
  (flush-transport tout)
  
  ((encoder-struct-begin pout) "person")

  ((encoder-field-begin pout) (field-header "name" type-string 1))
  ((encoder-string pout) "simon")
  ((encoder-field-end pout))

  ((encoder-field-begin pout) (field-header "age" type-byte 2))
  ((encoder-byte pout) 48)
  ((encoder-field-end pout))
  
  ((encoder-field-begin pout) (field-header "brilliant?" type-bool 3))
  ((encoder-boolean pout) #f)
  ((encoder-field-end pout))

  ((encoder-field-stop pout))
  ((encoder-struct-end pout))
  
  ((encoder-map-begin pout) (map-header type-string type-int32 3))
  ((encoder-string pout) "first")
  ((encoder-int32 pout) 101)
  ((encoder-string pout) "second")
  ((encoder-int32 pout) 102)
  ((encoder-string pout) "third")
  ((encoder-int32 pout) 103)
  ((encoder-map-end pout))
  
  ((encoder-message-end pout))
  (flush-transport tout)
  
  (close-transport tout)
  (define content (transport-output-bytes traw-out))
  (display (~bytes content))
  
  (define traw-in (open-input-memory-transport content))
  (define tin (if (false? wrapper-in) traw-in (wrapper-in traw-in)))
  (define pin (decoder tin))

  (cond
    [(false? pin)
     (displayln "warning: no decoder for protocol, skipping read checks")]
    [else
     (define msg ((decoder-message-begin pin)))
     (check-equal? (message-header-name msg) "test")
     (check-equal? (message-header-type msg) message-type-call)
     (check-equal? (message-header-sequence-id msg) 42)
     ((decoder-message-end pin))

     ((decoder-struct-begin pin))
     
     (define fld-1 ((decoder-field-begin pin)))
     (check-true (or (equal? (field-header-name fld-1) "")
                     (equal? (field-header-name fld-1) "name")))
     (check-equal? (field-header-type fld-1) type-string)
     (check-equal? (field-header-id fld-1) 1)
     (check-equal? ((decoder-string pin)) "simon")
     ((decoder-field-end pin))
     
     (define fld-2 ((decoder-field-begin pin)))
     (check-true (or (equal? (field-header-name fld-2) "")
                     (equal? (field-header-name fld-2) "age")))
     (check-equal? (field-header-type fld-2) type-byte)
     (check-equal? (field-header-id fld-2) 2)
     (check-equal? ((decoder-byte pin)) 48)
     ((decoder-field-end pin))
     
     (define fld-3 ((decoder-field-begin pin)))
     (check-true (or (equal? (field-header-name fld-3) "")
                     (equal? (field-header-name fld-3) "brilliant?")))
     (check-equal? (field-header-type fld-3) type-bool)
     (check-equal? (field-header-id fld-3) 3)
     (check-equal? ((decoder-boolean pin)) #f)
     ((decoder-field-end pin))

     ((decoder-field-stop pin))
     ((decoder-struct-end pin))
     
     (define a-map ((decoder-map-begin pin)))
     (check-equal? (map-header-key-type a-map) type-string)
     (check-equal? (map-header-element-type a-map) type-int32)
     (check-equal? (map-header-length a-map) 3)
     
     (check-equal? ((decoder-string pin)) "first")
     (check-equal? ((decoder-int32 pin)) 101)
     (check-equal? ((decoder-string pin)) "second")
     (check-equal? ((decoder-int32 pin)) 102)
     (check-equal? ((decoder-string pin)) "third")
     (check-equal? ((decoder-int32 pin)) 103)
     
     ((decoder-map-end pin))])

  (close-transport tin))

;; ---------- Test Cases

(define protocols
  (list (list "binary" make-binary-encoder make-binary-decoder)
        (list "compact" make-compact-encoder make-compact-decoder)
        (list "json" make-json-encoder make-json-decoder)
        (list "s-expression" make-sexpression-encoder make-sexpression-decoder)))

(define transport-wrappers
  (list (list "none" #f #f)
        (list "buffered" open-output-buffered-transport open-input-buffered-transport)
        (list "framed" open-output-framed-transport open-input-framed-transport)))

(for* ([protocol protocols] [transport transport-wrappers])
  (define test-name (format "simple test of ~a protocol over memory transport, with wrapper ~a"
                            (first protocol) (first transport)))
  (test-case test-name
   (displayln (format "running test: ~a" test-name))
   (apply test-write-then-read (append (rest protocol) (rest transport)))))
