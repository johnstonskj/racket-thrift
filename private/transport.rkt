#lang racket/base
;;
;; thrift - private/transport.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 (struct-out transport)
 (struct-out read-interceptor)
 (struct-out write-interceptor)
 (struct-out wrapped-transport))

;; ---------- Implementation (Types)

; basic transport, name of the transport type, the source (or string name)
; and the port to read/write.
(struct transport
  (name
   source
   [port #:mutable]))

(struct interceptor () #:transparent)

; allows for interception and routing of read functions in transport/common.
(struct read-interceptor interceptor
  (read-byte
   read-bytes
   read
   size
   position
   peek) #:transparent)

; allows for interception and routing of write functions in transport/common.
(struct write-interceptor interceptor
  (write-byte
   write-bytes
   write
   flush) #:transparent)

; a wrapped function includes the wrapped port and an interceptor (or #f).
(struct wrapped-transport transport
  (wrapped
   intercept))