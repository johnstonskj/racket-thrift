#lang racket/base
;;
;; thrift - transport/buffered.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 (contract-out
  
  [open-input-buffered-transport
   (-> input-transport? input-transport?)]

  [open-output-buffered-transport
   (-> output-transport? output-transport?)]

  [open-input-framed-transport
   (-> input-transport? input-transport?)]

  [open-output-framed-transport
   (-> output-transport? output-transport?)])

 buffered-read-length)

;; ---------- Requirements

(require racket/bool
         racket/class
         thrift/transport/common
         thrift/private/protocol
         thrift/private/logging)

;; ---------- Implementation

(define max-frame-size 16384000)
; Taken from https://github.com/apache/thrift/blob/master/doc/specs/thrift-rpc.md

(define buffered-read-length (make-parameter 512))

(define (open-input-buffered-transport tport)
  (make-object buffered-input-transport%
    tport #f))

(define (open-output-buffered-transport tport)
  (make-object buffered-output-transport%
    tport #f))

(define (open-input-framed-transport tport)
  (make-object buffered-input-transport%
    tport #t))

(define (open-output-framed-transport tport)
  (make-object buffered-output-transport%
    tport #t))

;; ---------- Internal class

(define buffered-input-transport%

  (class transport%

    ;; create this port as the local buffer, this means most underlying
    ;; operations act on the buffer, not the actual transport.
    (super-new
     [name (if framed? "framed" "buffered")]
     [source (if framed? 'read-framed 'read-buffered)]
     [port (open-input-bytes #"")])

    ;; this is the underlying transport, we read from it only to fill
    ;; the local buffer.
    (init-field data-transport
                framed?
                [read-buffer-size 512])

    (inherit-field name source port)

    (define/override (read-byte)
      (log-thrift-debug "~a:read-byte" (get-field name this))
      (when (eof-object? (peek-byte port))
        (read-buffer))
      (send data-transport read-byte))

    (define/override (read-bytes count)
      (log-thrift-debug "~a:read-bytes" name)
      (when (eof-object? (peek-byte port))
        (read-buffer))
      (send data-transport read-bytes count))

    (define/override (read)
      (log-thrift-debug "~a:read" name)
      (when (eof-object? (peek-byte port))
        (read-buffer))
      (send data-transport read))

    (define/override (peek)
      (log-thrift-debug "~a:peek" name)
      (define next-byte (peek-byte port))
      (cond
        [(eof-object? next-byte)
         (read-buffer)
         (send data-transport peek)]
        [else
         next-byte]))

    (define/private (read-buffer)
      (log-thrift-debug (format "~a:read-buffer from ~a"
                                name
                                (get-field source data-transport)))
      (close-input-port port)
      (define read-length
        (if framed?
            (read-plain-integer (get-field port data-transport) 4)
            read-buffer-size))
      (log-thrift-debug (format "~a:read-buffer, reading ~a bytes"
                                name
                                read-length))
      (define buffer (send data-transport read-bytes read-length))
      (set-field! port this (open-input-bytes buffer)))))

(define buffered-output-transport%

  (class transport%

    (super-new
     [name (if framed? "framed" "buffered")]
     [source (if framed? 'read-framed 'read-buffered)]
     [port (open-output-bytes)])

    (init-field data-transport
                framed?
                [max-length max-frame-size])

    (inherit-field name port)

    ;; TODO: check on write for buffer too big
    
    (define/override (flush)
      (log-thrift-debug "~a:flush" name)
      (close-output-port port)
      (define bytes (get-output-bytes port))
      (when (> (bytes-length bytes) 0)
        (cond
          [framed?
           (write-plain-integer data-transport (bytes-length bytes) 4)
           (send data-transport write-bytes bytes)]
          [else
           (send data-transport write-bytes bytes)])
        (send data-transport flush))
      (set-field! port this (open-output-bytes)))))
