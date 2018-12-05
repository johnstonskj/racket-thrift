#lang info
;;
;; Collection thrift.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(define collection "thrift")

(define pkg-desc "Read/Write Apache Parquet format files")
(define version "1.0")
(define pkg-authors '(johnstonskj))

(define deps '(
  "base"
  "http"
  "unix-socket-lib"
  "rackunit-lib"
  "racket-index"))
(define build-deps '(
  "scribble-lib"
  "racket-doc"
  "sandbox-lib"
  "cover-coveralls"))

(define scribblings '(("scribblings/thrift.scrbl" (multi-page))))

(define test-omit-paths '("scribblings" "private"))

(define racket-launcher-names (list "rthrift"))
(define racket-launcher-libraries (list "idl/generator.rkt"))
