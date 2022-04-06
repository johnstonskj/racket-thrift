#lang racket/base
;;
;; thrift - protocol/json.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 (contract-out
  
  [make-json-encoder
   (-> transport? (or/c protocol? #f))]
  
  [make-json-decoder
   (-> transport? (or/c protocol? #f))]))

;; ---------- Requirements

(require racket/bool
         racket/class
         racket/format
         racket/string
         net/base64
         thrift
         thrift/protocol/common
         thrift/protocol/exn-common
         thrift/private/protocol
         thrift/private/logging)

;; ---------- Implementation

; From <https://github.com/apache/thrift/blob/master/lib/cpp/src/thrift/protocol/TJSONProtocol.h>
;
; * Implements a protocol which uses JSON as the wire-format.
; *
; * Thrift types are represented as described below:
; *
; * 1. Every Thrift integer type is represented as a JSON number.
; *
; * 2. Thrift doubles are represented as JSON numbers. Some special values are
; *    represented as strings:
; *    a. "NaN" for not-a-number values
; *    b. "Infinity" for positive infinity
; *    c. "-Infinity" for negative infinity
; *
; * 3. Thrift string values are emitted as JSON strings, with appropriate
; *    escaping.
; *
; * 4. Thrift binary values are encoded into Base64 and emitted as JSON strings.
; *    The readBinary() method is written such that it will properly skip if
; *    called on a Thrift string (although it will decode garbage data).
; *
; *    NOTE: Base64 padding is optional for Thrift binary value encoding. So
; *    the readBinary() method needs to decode both input strings with padding
; *    and those without one.
; *
; * 5. Thrift structs are represented as JSON objects, with the field ID as the
; *    key, and the field value represented as a JSON object with a single
; *    key-value pair. The key is a short string identifier for that type,
; *    followed by the value. The valid type identifiers are: "tf" for bool,
; *    "i8" for byte, "i16" for 16-bit integer, "i32" for 32-bit integer, "i64"
; *    for 64-bit integer, "dbl" for double-precision loating point, "str" for
; *    string (including binary), "rec" for struct ("records"), "map" for map,
; *    "lst" for list, "set" for set.
; *
; * 6. Thrift lists and sets are represented as JSON arrays, with the first
; *    element of the JSON array being the string identifier for the Thrift
; *    element type and the second element of the JSON array being the count of
; *    the Thrift elements. The Thrift elements then follow.
; *
; * 7. Thrift maps are represented as JSON arrays, with the first two elements
; *    of the JSON array being the string identifiers for the Thrift key type
; *    and value type, followed by the count of the Thrift pairs, followed by a
; *    JSON object containing the key-value pairs. Note that JSON keys can only
; *    be strings, which means that the key type of the Thrift map should be
; *    restricted to numeric or string types -- in the case of numerics, they
; *    are serialized as strings.
; *
; * 8. Thrift messages are represented as JSON arrays, with the protocol
; *    version #, the message name, the message type, and the sequence ID as
; *    the first 4 elements.
; *
; * More discussion of the double handling is probably warranted. The aim of
; * the current implementation is to match as closely as possible the behavior
; * of Java's Double.toString(), which has no precision loss.  Implementors in
; * other languages should strive to achieve that where possible. I have not
; * yet verified whether std::istringstream::operator>>, which is doing that
; * work for me in C++, loses any precision, but I am leaving this as a future
; * improvement. I may try to provide a C component for this, so that other
; * languages could bind to the same underlying implementation for maximum
; * consistency.
; *

(define *protocol*
  (protocol-id "simple-json" 0 1))

(define json-protocol%
  
  (class protocol%

    (super-new [identity *protocol*]
               [state (json-state #f '())])

    (inherit-field input-transport
                   output-transport
                   state)

    (define/augment (message-begin message)
      ;; [<version>,"<name>",<type>,<sequence>, ...
      (set-json-state-in-map! state (cons #f (json-state-in-map state)))
      (send input-transport write-byte json-array-begin)
      (set-json-state-prefix! state #f)
      (write-number tport state (protocol-id-version *protocol*))
      (write-string tport state (message-header-name header))
      (write-number tport state (message-header-type header))
      (write-number tport state (message-header-sequence-id header)))

    
    (define/augment (message-end)
      (send output-transport flush))
    
    (define/augment (struct-begin name))
 
    (define/augment (struct-end))
    
    (define/augment (field-begin field))
 
    (define/augment (field-end))
    
    (define/augment (field-stop))
    
    (define/augment (map-begin map))
 
    (define/augment (map-end))
    
    (define/augment (list-begin list))
 
    (define/augment (list-end))
    
    (define/augment (set-begin set))
 
    (define/augment (set-end))
    
    (define/augment (boolean value))
 
    (define/augment (byte value))
 
    (define/augment (bytes value))
 
    (define/augment (int16 value))
 
    (define/augment (int32 value))
 
    (define/augment (int64 value))
 
    (define/augment (double value))
 
    (define/augment (string value))
 
    ;; Read API
    (define/augment (message-begin message-header? #f))
    
    (define/augment (message-end void? (void)))
    
    (define/augment (struct-begin string? ""))
 
    (define/augment (struct-end void? (void)))
    
    (define/augment (field-begin field-header? #f))
 
    (define/augment (field-end void? (void)))
    
    (define/augment (field-stop void? (void)))
    
    (define/augment (map-begin map-header? #f))
 
    (define/augment (map-end void? (void)))
    
    (define/augment (list-begin list-header? #f))
 
    (define/augment (list-end void? (void)))
    
    (define/augment (set-begin list-header? #f))
 
    (define/augment (set-end void? (void)))
    
    (define/augment (boolean boolean? #f))
 
    (define/augment (byte byte? 0))
 
    (define/augment (bytes bytes? #""))
 
    (define/augment (int16 integer? 0))
 
    (define/augment (int32 integer? 0))
 
    (define/augment (int64 integer? 0))
 
    (define/augment (double flonum? 0.0))
 
    (define/augment (string string? ""))))

;; ---------- Internal values/types

(define json-array-begin (char->integer #\[))
(define json-array-end (char->integer #\]))

(define json-object-begin (char->integer #\{))
(define json-object-end (char->integer #\}))

(define json-space (char->integer #\space))
(define json-elem-sep (char->integer #\,))
(define json-key-sep (char->integer #\:))

(define json-double-quote (char->integer #\"))

(define type-ident
  (hash type-bool #"\"tf\""
        type-byte #"\"i8\""
        type-int16 #"\"i16\""
        type-int32 #"\"i32\""
        type-int64 #"\"i64\""
        type-double #"\"dbl\""
        type-string #"\"str\""
        type-binary #"\"str\""
        type-struct #"\"rec\""
        type-map #"\"map\""
        type-list #"\"lst\""
        type-set #"\"set\""))

(define type-ident/reverse
  (for/hash ([(id str) type-ident])
    (values (string-trim (bytes->string/utf-8 str) "\"") id)))

(struct json-state
  ([prefix #:mutable]
   [in-map #:mutable]))

;; ---------- Internal procedures read/write

(define (read-message-begin tport state)
  (log-thrift-debug "~a:read-message-begin" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cons #f (json-state-in-map state)))
  (set-json-state-prefix! state #f)
  (define array-begin (transport-read-byte tport))
  (unless (equal? array-begin json-array-begin)
    (raise (decoding-error (current-continuation-marks) 'array-begin array-begin)))
  (define protocol-version (read-number tport state))
  (unless (equal? protocol-version (protocol-id-version *protocol*))
    (raise (invalid-protocol-version (current-continuation-marks) protocol-version)))
  (define msg-name (read-string tport state))
  (define msg-type (read-number tport state))
  (define msg-sequence (read-number tport state))
  (message-header msg-name msg-type msg-sequence))


(define (write-message-end tport state)
  (log-thrift-debug "~a:write-message-end" (protocol-id-string *protocol*))
  (transport-write-byte tport json-array-end)
  (flush-message-end tport))

(define (read-message-end tport state)
  (log-thrift-debug "~a:read-message-end" (protocol-id-string *protocol*)))

;; { ...
(define (write-struct-begin tport state name)
  (log-thrift-debug "~a:write-struct-begin: ~a" (protocol-id-string *protocol*) name)
  (set-json-state-in-map! state (cons #f (json-state-in-map state)))
  (write-prefix tport state)
  (transport-write-byte tport json-object-begin)
  (set-json-state-prefix! state #f))

(define (read-struct-begin tport state)
  (log-thrift-debug "~a:read-struct-begin" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cons #f (json-state-in-map state)))
  (read-prefix tport state)
  (read-byte/expecting tport json-object-begin)
  (set-json-state-prefix! state #f))


;; ... }
(define (write-struct-end tport state)
  (log-thrift-debug "~a:write-struct-end" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cdr (json-state-in-map state)))
  (transport-write-byte tport json-object-end)
  (write-ifmap-element-suffix tport state))

(define (read-struct-end tport state)
  (log-thrift-debug "~a:read-struct-end" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cdr (json-state-in-map state)))
  (read-byte/expecting tport json-object-end))


;; ["<key-type>","<element-type>",len, ...
(define (write-map-begin tport state header)
  (log-thrift-debug "~a:write-map-begin: ~a" (protocol-id-string *protocol*) header)
  (write-prefix tport state)
  (set-json-state-prefix! state #f)
  (transport-write-byte tport json-array-begin)
  (transport-write-bytes tport (hash-ref type-ident (map-header-key-type header)))
  (transport-write-byte tport json-elem-sep)
  (transport-write-bytes tport (hash-ref type-ident (map-header-element-type header)))
  (transport-write-byte tport json-elem-sep)
  (write-number tport state (map-header-length header))
  (set-json-state-in-map! state (cons 1 (json-state-in-map state))))

(define (read-map-begin tport state)
  (log-thrift-debug "~a:read-map-begin" (protocol-id-string *protocol*))
  (read-prefix tport state)
  (read-byte/expecting tport json-array-begin)
  (set-json-state-prefix! state #f)
  (define key-type (read-string tport state))
  (define element-type (read-string tport state))
  (define map-length (read-number tport state))
  (set-json-state-in-map! state (cons 1 (json-state-in-map state)))
  (map-header (hash-ref type-ident/reverse key-type)
              (hash-ref type-ident/reverse element-type)
              map-length))

;; ... ]
(define (write-map-end tport state)
  (log-thrift-debug "~a:write-map-end" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cdr (json-state-in-map state)))
  (transport-write-byte tport json-array-end)
  (write-ifmap-element-suffix tport state))

(define (read-map-end tport state)
  (log-thrift-debug "~a:read-map-end" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cdr (json-state-in-map state)))
  (read-byte/expecting tport json-array-end))


;; ["<element-type>",len, ...
(define (write-list-begin tport state header)
  (log-thrift-debug "~a:write-list-begin: ~a" (protocol-id-string *protocol*) header)
  (set-json-state-in-map! state (cons #f (json-state-in-map state)))
  (write-prefix tport state)
  (transport-write-byte tport json-array-begin)
  (transport-write-bytes tport (hash-ref type-ident (list-or-set-element-type header)))
  (write-number tport state (list-or-set-length header)))

(define (read-list-begin tport state)
  (log-thrift-debug ":read-list-begin" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cons #f (json-state-in-map state)))
  (read-prefix tport state)
  (read-byte/expecting tport json-array-begin)
  (set-json-state-prefix! state #f)
  (define element-type (read-string tport state))
  (define list-length (read-number tport state))
  (list-or-set (hash-ref type-ident/reverse element-type) list-length))


;; ... ]
(define (write-list-end tport state)
  (log-thrift-debug "~a:write-list-end" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cdr (json-state-in-map state)))
  (transport-write-byte tport json-array-end)
  (write-ifmap-element-suffix tport state))

(define (read-list-end tport state)
  (log-thrift-debug "~a:read-list-end" (protocol-id-string *protocol*))
  (set-json-state-in-map! state (cdr (json-state-in-map state)))
  (read-byte/expecting tport json-array-end)
  (read-ifmap-element-suffix tport state))


;; "<field-id>":{"<type>": ...
(define (write-field-begin tport state header)
  (log-thrift-debug "~a:write-field-begin: ~a" (protocol-id-string *protocol*) header)
  (write-prefix tport state)
  (set-json-state-prefix! state #f)
  (transport-write-bytes tport
                         (string->bytes/utf-8 (format "\"~a\"" (field-header-id header))))
  (transport-write-byte tport json-key-sep)
  (transport-write-byte tport json-object-begin)
  (transport-write-bytes tport (hash-ref type-ident (field-header-type header)))
  (transport-write-byte tport json-key-sep)
  (set-json-state-prefix! state #f))

(define (read-field-begin tport state)
  (log-thrift-debug "~a:read-field-begin" (protocol-id-string *protocol*))
  (define key (read-string tport state))
  (skip-over tport json-key-sep)
  (skip-over tport json-object-begin)
  (set-json-state-prefix! state #f)
  (define type (read-string tport state))
  (skip-over tport json-key-sep)
  (set-json-state-prefix! state #f)
  (field-header "" (hash-ref type-ident/reverse type) (string->number key)))

;; ... }
(define (write-field-end tport state)
  (log-thrift-debug "~a:write-field-end" (protocol-id-string *protocol*))
  (transport-write-byte tport json-object-end))

(define (read-field-end tport state)
  (log-thrift-debug "~a:read-field-end" (protocol-id-string *protocol*))
  (read-byte/expecting tport json-object-end))

;; plain JSON boolean
(define (write-boolean tport state bool)
  (log-thrift-debug "~a:write-boolean: ~a" (protocol-id-string *protocol*) bool)
  (write-prefix tport state)
  (write-ifmap-element-prefix tport state)
  (if (false? bool)
      (transport-write-bytes tport #"false")
      (transport-write-bytes tport #"true"))
  (write-ifmap-element-sep tport state)
  (write-ifmap-element-suffix tport state))

(define (read-boolean tport state)
  (log-thrift-debug "~a:read-boolean" (protocol-id-string *protocol*))
  (read-prefix tport state)
  (read-ifmap-element-prefix transport state)
  (define bool (read-atom tport))
  (define result (cond
                   [(bytes=? bool #"false") #f]
                   [(bytes=? bool #"true") #t]
                   [else
                    (raise (decoding-error
                            (current-continuation-marks)
                            "boolean"
                            bool))]))
  (read-ifmap-element-sep transport state)
  (read-ifmap-element-suffix transport state)
  result)


;; plain JSON number
(define (write-number tport state num)
  (log-thrift-debug "~a:write-number: ~a" (protocol-id-string *protocol*) num)
  (write-prefix tport state)
  (define write-as-key (write-ifmap-element-prefix tport state))
  (if write-as-key
      (transport-write-bytes tport
                             (string->bytes/utf-8 (format "\"~a\"" num)))
      (transport-write-bytes tport
                             (string->bytes/utf-8 (~a num))))
  (write-ifmap-element-sep tport state)
  (write-ifmap-element-suffix tport state))

(define (read-number tport state)
  (log-thrift-debug "~a:read-number" (protocol-id-string *protocol*))
  (read-prefix tport state)
  (read-ifmap-element-prefix tport state)
  (define atom (read-atom tport))
  (define result (string->number (bytes->string/utf-8 atom)))
  (read-ifmap-element-sep tport state)
  (read-ifmap-element-suffix tport state)
  result)


;; plain JSON string
(define (write-string tport state str)
  (log-thrift-debug "~a:write-string: ~a" (protocol-id-string *protocol*) str)
  (write-prefix tport state)
  (write-ifmap-element-prefix tport state)
  (transport-write-bytes tport
                         (string->bytes/utf-8 (~s str)))
  (write-ifmap-element-sep tport state)
  (write-ifmap-element-suffix tport state))

(define (read-string tport state)
  (log-thrift-debug "~a:read-string" (protocol-id-string *protocol*))
  (read-prefix tport state)
  (read-ifmap-element-prefix tport state)
  (define result (string-trim (bytes->string/utf-8 (read-atom tport)) "\""))
  (read-ifmap-element-sep tport state)
  (read-ifmap-element-suffix tport state)
  result)

;; base-64 encoded JSON string
(define (write-binary tport state bytes)
  (log-thrift-debug "~a:write-binary" (protocol-id-string *protocol*))
  (write-prefix tport state)
  (write-ifmap-element-prefix tport state)
  (transport-write-byte tport json-double-quote)
  (transport-write-bytes tport (base64-encode bytes ""))
  (transport-write-byte tport json-double-quote)
  (write-ifmap-element-sep tport state)
  (write-ifmap-element-suffix tport state))

(define (read-binary tport state)
  (log-thrift-debug "~a:read-binary" (protocol-id-string *protocol*))
  (read-prefix tport state)
  (read-ifmap-element-prefix tport state)
  (read-byte/expecting tport json-double-quote)
  (define bytes-read
    (let next-byte ([bytestr #""] [a-byte (transport-peek tport)])
      (cond
        [(= a-byte json-double-quote)
         bytestr]
        [else
         (define another-byte (transport-read-byte tport))
         (next-byte (bytes-append bytestr (bytes another-byte)) (transport-peek tport))])))
  (define result (base64-decode bytes-read))
  (read-byte/expecting tport json-double-quote)
  (write-ifmap-element-sep tport state)
  (write-ifmap-element-suffix tport state)
  result)

(define (read-atom tport)
  (let next-byte ([bytestr #""]
                   [a-byte (transport-peek tport)])
    (cond
      [(or (= a-byte json-elem-sep)
           (= a-byte json-key-sep)
           (= a-byte json-array-end)
           (= a-byte json-object-end)
           (eof-object? a-byte))
       bytestr]
      [else
       (define another-byte (transport-read-byte tport))
       (next-byte (bytes-append bytestr (bytes another-byte)) (transport-peek tport))])))

;; ---------- Internal procedures read/write state

(define (read-byte/expecting tport expecting)
  (define value (transport-read-byte tport))
  (unless (= value expecting)
    (raise (decoding-error (current-continuation-marks)
                           (integer->char expecting)
                           (integer->char value)))))

(define (write-prefix tport state)
  ; write a "," as a prefix to an output value
  ; depends on the current prefix state
  (cond
    [(false? (json-state-prefix state))
       (set-json-state-prefix! state #t)]
    [else
     (transport-write-byte tport json-elem-sep)]))

(define (read-prefix tport state)
  (cond
    [(false? (json-state-prefix state))
       (set-json-state-prefix! state #t)]
    [else
     (skip-over tport json-elem-sep)]))


(define (write-ifmap-element-prefix tport state)
  (cond
    [(equal? (car (json-state-in-map state)) 1)
     (transport-write-byte tport json-object-begin)
     (set-json-state-prefix! state #f)
     #t]
    [else
     #f]))

(define (read-ifmap-element-prefix tport state)
  (cond
    [(equal? (car (json-state-in-map state)) 1)
     (read-byte/expecting tport json-object-begin)
     (set-json-state-prefix! state #f)
     #t]
    [else
     #f]))


(define (write-ifmap-element-sep tport state)
  (when (equal? (car (json-state-in-map state)) 1)
    (transport-write-byte tport json-key-sep)
    (set-json-state-in-map! state
                            (cons 2 (cdr (json-state-in-map state))))
    (set-json-state-prefix! state #f)))

(define (read-ifmap-element-sep tport state)
  (when (equal? (car (json-state-in-map state)) 1)
    (read-byte/expecting tport json-key-sep)
    (set-json-state-in-map! state
                            (cons 2 (cdr (json-state-in-map state))))
    (set-json-state-prefix! state #f)))


(define (write-ifmap-element-suffix tport state)
  (define in-map-state (json-state-in-map state))
  (cond
    [(equal? (car in-map-state) 3)
     (set-json-state-in-map! state
                             (cons 1 (cdr (json-state-in-map state))))
     (transport-write-byte tport json-object-end)]
    [(number? (car in-map-state))
     (set-json-state-in-map! state
                             (cons (add1 (car in-map-state))
                                   (cdr in-map-state)))]))

(define (read-ifmap-element-suffix tport state)
  (define in-map-state (json-state-in-map state))
  (cond
    [(equal? (car in-map-state) 3)
     (set-json-state-in-map! state
                             (cons 1 (cdr (json-state-in-map state))))
     (read-byte/expecting tport json-object-end)]
    [(number? (car in-map-state))
     (set-json-state-in-map! state
                             (cons (add1 (car in-map-state))
                                   (cdr in-map-state)))]))

;; ---------- Internal procedures skip characters

(define (skip-over tport skip-char [and-spaces #t])
  (define result (skip-to tport skip-char))
  (cond
    [(equal? result #t)
     (transport-read-byte tport)
     (when (equal? and-spaces #t)
       (skip-spaces tport))]
    [else result]))

(define (skip-spaces tport)
  (let next-char ([peeked (transport-peek tport)])
    (cond
      [(equal? peeked json-space)
       (transport-read-byte tport)
       (next-char (transport-peek tport))]
      [(eof-object? peeked)
       eof]
      [else
       #t])))
  
(define (skip-to tport skip-char)
  (let next-char ([peeked (transport-peek tport)])
    (cond
      [(equal? peeked skip-char)
       #t]
      [(eof-object? peeked)
       eof]
      [else
       (transport-read-byte tport)
       (next-char (transport-peek tport))])))
