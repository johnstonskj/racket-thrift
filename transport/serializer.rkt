#lang racket/base
;;
;; thrift - transport/memory.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 (contract-out
  
  [make-serializer
   (-> encoder? serializer?)]

  [serialize
   (->* (serializer? any/c) (type?) string?)]

  [deserialize
   (->* (serializer?) (type?) any/c)]))

;; ---------- Requirements

(require racket/bool
         racket/list
         racket/set
         thrift
         thrift/transport/memory
         thrift/private/logging)

;; ---------- Implementation

(struct serializer (transport protocol))

(define (make-serializer protocol)
  (define t (open-output-memory-transport))
  (define p (protocol t))
  (serializer t p))

(define (serialize serial v [fh #f])
  (define out (transport-port (serializer-transport serial)))
  (define encode (serializer-protocol serial))
  (cond
    [(boolean? v)
     ((encoder-boolean encode) v)]
    [(byte? v)
     ((encoder-byte encode) v)]
    [(bytes? v)
     ((encoder-bytes encode) v)]
    [(flonum? v)
     ((encoder-double encode) v)]
    [(integer? v)
     ((encoder-int32 encode) v)]
    [(string? v)
     ((encoder-string encode) v)]
    [(list? v)
     (define actual (cond
                      [(and (false? fh) (empty? v))
                       -1]
                      [(and (false? fh) (not (empty? v)))
                       (value->type (car v))]
                      [else (field-header-type fh)]))
     ((encoder-list-begin encode)
       (list-or-set actual (length v)))
     (for ([e v])
       (serialize serial e (field-header "" actual 0)))
     ((encoder-list-end encode))]
    [(set? v)
;     (define actual (cond
;                      [(and (false? fh) (set-empty? v))
;                       -1]
;                      [(and (false? fh) (not (set-empty? v)))
;                       (value->type (car v))]
;                      [else (field-header-type fh)]))
;     ((encoder-set-begin)
;      (list-or-set actual (set-length v)))
;     (for ([e v])
;       (->string serial e (field-header "" actual 0)))
     ((encoder-set-end))]
    [(struct? v)
     ((encoder-struct-begin))
     ((encoder-double encode) v)
     ((encoder-struct-end))]
    [else (error "cannot serialize v")])
; (transport-flush (serializer-transport serial))
  (bytes->string/utf-8 (get-output-bytes out)))

(define (deserialize serial [fh #f])
  #f)

;; ---------- Internal Procedures

(define (value->type v)
  (cond
    [(boolean? v)  type-bool]
    [(flonum? v)   type-double]
    [(integer? v)  type-int32]
    [(byte? v)     type-byte]
    [(bytes? v)    type-binary] 
    [(string? v)   type-string]
    [(list? v)     type-list]
    [(set? v)      type-set]
    [(hash? v)     type-map]
    [(struct? v)   type-struct]
    [else (error "cannot serialize v")]))

;; ---------- Internal Tests

(require thrift/protocol/sexpression)

(define s (make-serializer make-sexpression-encoder))

(serialize s '(101 202))