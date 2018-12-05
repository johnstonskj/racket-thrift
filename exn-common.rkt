#lang racket/base
;;
;; thrift - exn-common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 (all-defined-out))

(struct exn:thrift exn:fail
  (type) #:transparent)

(struct exn:thrift:transport exn:thrift
  () #:transparent)

(struct exn:thrift:protocol exn:thrift
  () #:transparent)