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

  (send pout write-message-begin (message-header "test" message-type-call 42))
  (send pout flush)
  
  (send pout write-struct-begin "person")

  (send pout write-field-begin (field-header "name" type-string 1))
  (send pout write-string "simon")
  (send pout write-field-end)

  (send pout write-field-begin (field-header "age" type-byte 2))
  (send pout write-byte 48)
  (send pout write-field-end pout)
  
  (send pout write-field-begin (field-header "brilliant?" type-bool 3))
  (send pout write-boolean #f)
  (send pout write-field-end pout)

  (send pout write-field-stop)
  (send pout write-struct-end)
  
  (send pout write-map-begin (map-header type-string type-int32 3))
  (send pout write-string "first")
  (send pout write-int32 101)
  (send pout write-string "second")
  (send pout write-int32 102)
  (send pout write-string "third")
  (send pout write-int32 103)
  (send pout write-map-end)
  
  (send pout write-message-end)
  (send pout write-flush)
  
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
     (define msg (send pin read-message-begin))
     (check-equal? (message-header-name msg) "test")
     (check-equal? (message-header-type msg) message-type-call)
     (check-equal? (message-header-sequence-id msg) 42)
     (send pin read-message-end)

     (send pin read-struct-begin)
     
     (define fld-1 (send pin read-field-begin))
     (check-true (or (equal? (field-header-name fld-1) "")
                     (equal? (field-header-name fld-1) "name")))
     (check-equal? (field-header-type fld-1) type-string)
     (check-equal? (field-header-id fld-1) 1)
     (check-equal? (send pin read-string) "simon")
     (send pin read-field-end)
     
     (define fld-2 (send pin read-field-begin))
     (check-true (or (equal? (field-header-name fld-2) "")
                     (equal? (field-header-name fld-2) "age")))
     (check-equal? (field-header-type fld-2) type-byte)
     (check-equal? (field-header-id fld-2) 2)
     (check-equal? (send pin read-byte) 48)
     (send pin read-field-end)
     
     (define fld-3 (send pin read-field-begin))
     (check-true (or (equal? (field-header-name fld-3) "")
                     (equal? (field-header-name fld-3) "brilliant?")))
     (check-equal? (field-header-type fld-3) type-bool)
     (check-equal? (field-header-id fld-3) 3)
     (check-equal? (send pin read-boolean) #f)
     (send pin read-field-end)

     (send pin read-field-stop)
     (send pin read-struct-end)
     
     (define a-map (send pin read-map-begin))
     (check-equal? (map-header-key-type a-map) type-string)
     (check-equal? (map-header-element-type a-map) type-int32)
     (check-equal? (map-header-length a-map) 3)
     
     (check-equal? (send pin read-string) "first")
     (check-equal? (send pin read-int32) 101)
     (check-equal? (send pin read-string) "second")
     (check-equal? (send pin read-int32) 102)
     (check-equal? (send pin read-string) "third")
     (check-equal? (send pin read-int32) 103)
     
     (send pin read-map-end)

     (send pin read-message-end)])

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
