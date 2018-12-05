#lang racket/base
;;
;; thrift - transport/common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide
 
 (contract-out

  [transport?
   (-> any/c boolean?)]

  [transport-source
   (-> transport? (or/c symbol? string?))]

  [transport-port
   (-> transport? port?)]

  [transport-peek
   (-> transport? byte?)]
  
  [transport-read-byte
   (-> transport? byte?)]
  
  [transport-read-bytes
   (-> transport? exact-positive-integer? bytes?)]
  
  [transport-read
   (-> transport? any/c)]
  
  [transport-write-byte
   (-> transport? byte? void?)]
  
  [transport-write-bytes
   (->* (transport? bytes?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  
  [transport-write
   (-> transport? any/c void?)]
  
  [transport-size
   (-> transport? (or/c exact-nonnegative-integer? eof-object?))]

  [transport-read-position
   (->* (transport?) (exact-nonnegative-integer?) (or/c exact-nonnegative-integer? eof-object?))]

  [input-transport?
   (-> transport? boolean?)]
  
  [output-transport?
   (-> transport? boolean?)]

  [flush-transport
   (-> output-transport? void?)]
  
  [close-transport
   (-> transport? any/c)])
 
 transport)

;; ---------- Requirements

(require racket/bool
         racket/struct
         thrift/common
         thrift/private/transport)

;; ---------- Implementation

(define (transport-read-byte tport)
  (define actual (get-wrapped-func tport 0))
  (if (false? actual)
      (read-byte (transport-port tport))
      (actual tport)))
  
(define (transport-read-bytes tport amt)
  (define actual (get-wrapped-func tport 1))
  (if (false? actual)
      (read-bytes amt (transport-port tport))
      (actual tport amt)))
  
(define (transport-read tport)
  (define actual (get-wrapped-func tport 2))
  (if (false? actual)
      (read (transport-port tport))
      (actual tport)))

(define (transport-size tport)
  (cond
    [(input-transport? tport)
     (define actual (get-wrapped-func tport 3))
     (if (false? actual)
         (file-size (transport-source tport))
         (actual tport))]
    [else eof]))

(define (transport-read-position tport [new-pos #f])
  (define actual (get-wrapped-func tport 4))
  (define position (if (false? actual) file-position actual))
  (cond
    [(input-transport? tport)
     (cond
       [(false? new-pos)
        (position (transport-port tport))]
       [else
        (position (transport-port tport) new-pos)
        new-pos])]
    [else eof]))

(define (transport-peek tport)
  (define actual (get-wrapped-func tport 5))
  (if (false? actual)
      (peek-byte (transport-port tport))
      (actual tport)))
  
(define (transport-write-byte tport b)
  (define actual (get-wrapped-func tport 0))
  (if (false? actual)
      (write-byte b (transport-port tport))
      (actual tport b)))
  
(define (transport-write-bytes tport bs [start 0] [end #f])
  (define the-end (if (false? end) (bytes-length bs) end))
  (define actual (get-wrapped-func tport 1))
  (if (false? actual)
      (if (false? end)
          (write-bytes bs (transport-port tport) start)
          (write-bytes bs (transport-port tport) start end))
      (actual tport bs start end))
  (void))
  
(define (transport-write tport v)
  (define actual (get-wrapped-func tport 2))
  (if (false? actual)
      (write v (transport-port tport))
      (actual (transport-port tport) v)))
  
(define (flush-transport tport)
  (define actual (get-wrapped-func tport 3))
  (if (false? actual)
      (flush-output (transport-port tport))
      (actual tport)))

(define (input-transport? tport)
  (input-port? (transport-port tport)))

(define (output-transport? tport)
  (output-port? (transport-port tport)))

(define (close-transport tport)
  (define p (if (wrapped-transport? tport)
                (transport-port (wrapped-transport-wrapped tport))
                (transport-port tport)))
  (cond
    [(input-port? p)
     (close-input-port p)]
    [(output-port? p)
     (flush-transport tport)
     (close-output-port p)]
    [else (error "what kind of port is this? " p)]))

;; ---------- Internal procedures

(define (get-wrapped-func tport findex)
  (cond
    [(wrapped-transport? tport)
     (list-ref (struct->list (wrapped-transport-intercept tport)) findex)]
    [else #f]))
