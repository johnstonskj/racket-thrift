#lang racket/base
;;
;; thrift - idl/common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 (except-out (all-defined-out)
             type-unused-1
             type-unused-2
             type-unused-3))

;; ---------- Requirements

(require thrift/private/enumeration)

;; ---------- Implementation (Types)

;; from https://thrift.apache.org/docs/types
(define-enumeration type 0
  (;; Base Types
   stop
   void
   bool
   byte
   double
   unused-1
   int16
   unused-2
   int32
   unused-3
   int64
   string
   struct
   map
   set
   list
   utf-8
   utf-16))

(define type-int8 type-byte)
(define type-utf-7 type-string)
(define type-binary type-string)

(define-enumeration required-type
  (required
   optional
   default))

(define-enumeration container-type
  (list-of
   set-of
   map-of
   none))

(struct thrift-field
  (id
   name
   required
   container
   major-type ; one day will be (decoder . encoder)
   minor-type ; one day will be (decoder . encoder)
   [position #:mutable])
  #:transparent)
