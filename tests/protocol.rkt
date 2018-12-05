#lang racket/base
;;
;; thrift - prototol.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

;; ---------- Requirements

(require rackunit
         ; ---------
         thrift
         thrift/protocol/binary
         thrift/protocol/compact
         thrift/protocol/json
         thrift/protocol/sexpression
         thrift/transport/memory)

;; ---------- Test Fixtures

;; ---------- Internal procedures

;; ---------- Test Cases

(define encoding-tests
  (hash make-binary-encoder #"\1\200\0\a\5\0\0\0mthod\t\0\0\0\v\1\0\5\0\0\0simon\3\2\0000\2\3\0\0\v\b\3\0\0\0\5\0\0\0firste\0\0\0\6\0\0\0secondf\0\0\0\5\0\0\0thirdg\0\0\0"
        ;make-compact-encoder #""
        make-json-encoder #"[1,\"mthod\",7,9,{\"1\":{\"str\":\"simon\"},\"2\":{\"i8\":48},\"3\":{\"tf\":false}},[\"str\",\"i32\",3,{\"first\":101},{\"second\":102},{\"third\":103}]]"
        make-sexpression-encoder #"#s(protocol-header \"s-expression\" 1 #s(message-header \"mthod\" 7 9)) #s(field-header \"name\" 11 1) \"simon\" #s(field-header \"age\" 3 2) 48 #s(field-header \"brilliant?\" 2 3) #f #s(map-header 11 8 3) \"first\" 101 \"second\" 102 \"third\" 103 "))
   
  
(for ([(encoder results) encoding-tests])
  (test-case
   (format "testing simple encoding using ~a" encoder)
   (define t (open-output-memory-transport))
   (define p (encoder t))

   ((encoder-message-begin p) (message-header "mthod" 7 9))

   ((encoder-struct-begin p) "mthod_args")

   ((encoder-field-begin p) (field-header "name" type-string 1))
   ((encoder-string p) "simon")
   ((encoder-field-end p))

   ((encoder-field-begin p) (field-header "age" type-byte 2))
   ((encoder-byte p) 48)
   ((encoder-field-end p))

   ((encoder-field-begin p) (field-header "brilliant?" type-bool 3))
   ((encoder-boolean p) #f)
   ((encoder-field-end p))

   ((encoder-struct-end p))

   ((encoder-map-begin p) (map-header type-string type-int32 3))
   ((encoder-string p) "first")
   ((encoder-int32 p) 101)
   ((encoder-string p) "second")
   ((encoder-int32 p) 102)
   ((encoder-string p) "third")
   ((encoder-int32 p) 103)
   ((encoder-map-end p))
 
   ((encoder-message-end p))

   (define actual (transport-output-bytes t))

   (check-equal? actual results)))