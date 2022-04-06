#lang racket/base
;;
;; thrift - protocol/wrapper.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 
 protocol-wrapper%)

;; ---------- Requirements

(require racket/class
         thrift/protocol/common
         thrift/private/protocol-class)

;; ---------- Implementation (Types)

(define protocol-wrapper%
  
  (class protocol%

    (super-new)

    (init-field wrapped)

    (define-wrapped
      [write-message-begin message]
      write-message-end
      [write-struct-begin name]
      write-struct-end
      [write-field-begin field]
      write-field-end
      write-field-stop
      [write-map-begin map]
      write-map-end
      [write-list-begin list]
      write-list-end
      [write-set-begin set]
      write-set-end
      [write-boolean value]
      [write-byte value]
      [write-bytes value]
      [write-int16 value]
      [write-int32 value]
      [write-int64 value]
      [write-double value]
      [write-string value]
      
      read-message-begin
      read-message-end
      read-struct-begin
      read-struct-end
      read-field-begin
      read-field-end
      read-field-stop
      read-map-begin
      read-map-end
      read-list-begin
      read-list-end
      read-set-begin
      read-set-end
      read-boolean
      read-byte
      read-bytes
      read-int16
      read-int32
      read-int64
      read-double
      read-string)))