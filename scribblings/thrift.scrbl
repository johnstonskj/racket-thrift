#lang scribble/manual

@(require racket/file scribble/core)

@;{============================================================================}

@title[#:version "1.0"]{Package thrift.}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

Support for the @hyperlink["https://thrift.apache.org/"]{Apache Thrift}
framework, including @hyperlink["https://thrift.apache.org/docs/idl"]{IDL},
@hyperlink["https://thrift.apache.org/docs/types"]{type system}, transport,
protocol, and processor (see @hyperlink["https://thrift.apache.org/docs/concepts"]{Thrift
Concepts}).

Note that features added and supported by this package do not follow the set
of required and recommended sets described in
@hyperlink["https://thrift.apache.org/docs/HowToNewLanguage"]{Roadmap for adding
new language bindings}.

@table-of-contents[]

@include-section["architecture.scrbl"]

@include-section["idl.scrbl"]

@include-section["transport.scrbl"]

@include-section["protocol.scrbl"]

@include-section["processor.scrbl"]

@section{License}

@verbatim|{|@file->string["../LICENSE"]}|