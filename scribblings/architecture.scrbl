#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          racket/contract
          thrift
          (for-label racket/base
                     racket/contract
                     thrift
                     thrift/idl/language))

@;{============================================================================}
@(define example-eval (make-base-eval
                      '(require racket/string
                                racket/contract
                                thrift)))

@;{============================================================================}
@title[]{Thrift for Racket Architecture}

The @hyperlink["https://thrift.apache.org/docs/concepts"]{Apache Thrift Concepts}
page introduces the following diagram showing the architectural layers that
make up the Thrift framework. The documentation below will describe how these
layers and the features within them are provided by this package.

@centered{
@verbatim|{
+-------------------------------------------+
| Server                                    |
| (single-threaded, event-driven etc)       |
+-------------------------------------------+
| Processor                                 |
| (compiler generated)                      |
+-------------------------------------------+
| Protocol                                  |
| (JSON, compact etc)                       |
+-------------------------------------------+
| Transport                                 |
| (raw TCP, HTTP etc)                       |
+-------------------------------------------+
}|
}

Additionally, the Thrift
@hyperlink["https://thrift.apache.org/docs/HowToNewLanguage"]{@italic{Roadmap for
adding new language bindings}} specifies required, recommended, and optional
features for each of the architecture layers above.

Finally, there is no attempt to utilize the existing and comprehensive
@hyperlink["https://github.com/apache/thrift/blob/master/test/README.md"]{integration
test suite}.

@;{============================================================================}
@section[]{Interface Definition Language}



The tools provided in this package do not, currently, provide a parser for the
@hyperlink["https://thrift.apache.org/docs/idl"]{Thrift interface description language}
as-is. Instead it uses a set of macros in the module @racket[thrift/idl/language]
to describe an interface and can code generate from a module that uses these
macros.

The following table lists the major components of the Thrift IDL language and
their corresponding Racket macros.

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Thrift}   @bold{Racket})
               (list "include"       "not yet defined")
               (list "namespace"     @racket[define-thrift-namespace])
               (list "const"         "not yet defined")
               (list "typedef"       "not yet defined")
               (list "enum"          @racket[define-thrift-enum])
               (list "senum"         "not yet defined")
               (list "struct"        @racket[define-thrift-struct])
               (list "union"         @racket[define-thrift-union])
               (list "exception"     "not yet defined")
               (list "service"       "not yet defined"))]


@;{============================================================================}
@section[]{Transport Layer}

The transport layer consists of wrappers around standard Racket I/O capabilities
and specifically a @racket[transport] struct that wraps an individual Racket
@racket[port?]. Each transport is responsible for providing an @racket[open-input...]
and @racket[open-output...] function. As you can see from the table below their
is good coverage of the transport API, although at this time the two server
functions @italic{listen} and @italic{accept} are not supported.

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Thrift}   @bold{Racket})
               (list "open"          @racket[open-input-?-transport])
               (list ""              @racket[open-output-?-transport])
               (list "close"         @racket[close-transport])
               (list "read"          @racket[transport-read-byte])
               (list ""              @racket[transport-read-bytes])
               (list "write"         @racket[transport-write-byte])
               (list ""              @racket[transport-write-bytes])
               (list "flush"         @racket[flush-transport])
               (list "listen"        "not yet defined")
               (list "accept"        "not yet defined"))]

The following table lists the transports defined by Thrift and their corresponding
Racket implementation (if any).

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Feature} @bold{Coverage}       @bold{Racket})
               (list "Sockets"      "minimal-required"    "not yet defined")
               (list "Buffered"     "minimal-required"
                     @secref["Buffered_and_Framed_Transports" #:doc
                             '(lib "thrift/scribblings/thrift.scrbl")])
               (list "Framed"       "minimal-required"
                     @secref["Buffered_and_Framed_Transports" #:doc
                             '(lib "thrift/scribblings/thrift.scrbl")])
               (list "Files"        "minimal-recommended"
                     @secref["File_Transport" #:doc '(lib "thrift/scribblings/thrift.scrbl")])
               (list "HTTP Client"  "minimal-recommended" "not yet defined")
               (list "HTTP Server"  "other-recommended"   "not yet defined")
               (list "Pipes"        "other-recommended"   "not yet defined")
               (list "NamedPipes"   "other-recommended"   "not yet defined")
               (list ""             ""
                     @secref["In-Memory_Transport" #:doc '(lib "thrift/scribblings/thrift.scrbl")])
               (list ""             ""
                     @secref["Console_Transport" #:doc '(lib "thrift/scribblings/thrift.scrbl")]))]

The memory buffer transport (module @racket[thrift/transport/memory]) is primarily
used for testing and allows reading/writing over byte buffers.

@;{============================================================================}
@section[]{Protocol Layer}

The Protocol interface described in the Thrift Concepts page is actually split
into two separate components in the Racket implementation. These two are
defined in the module @racket[thrift/protocol/common] as the structs
@racket[encoder] and @racket[decoder]. This reflects the Racket philosophy of
having separate input and output ports as well as making it easier to implement
just one half of the protocol interface.

The following table presents a sample of the mapping from the Thrift API on the
left hand side to the Racket module form on the right. For the four methods on
the thrift interface that deal with structured data: message, field, map, list,
and set we define structs that are input to the write functions and returned
from the read functions.

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Thrift}   @bold{Racket})
               (list "writeMessageBegin(name, type, seq)"
                     @racket[(write-message-begin message-header?)])
               (list "name, type, seq = readMessageBegin()"
                     @racket[read-message-begin -> message-header?])
               (list "writeMessageEnd()"
                     @racket[write-message-end])
               (list "readMessageEnd()"
                     @racket[read-message-end])
               (list "writeI16(i16)"
                     @racket[(write-int16 type-int16?)])
               (list "i16 = readI16()"
                     @racket[read-int16 -> type-int16?]))]

Additionally the @racket[thrift/protocol/decoding] module provides
@racket[decode-a-struct], @racket[decode-a-list], and @racket[decode-a-set].

The following table lists the protocols defined by Thrift and their corresponding
Racket implementation (if any).

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Feature} @bold{Coverage}       @bold{Racket})
               (list "Binary"       "minimal-required"
                     @secref["Binary_Protocol" #:doc
                             '(lib "thrift/scribblings/thrift.scrbl")])
               (list "Multiplex"    "minimal-required"
                     @secref["Multiplexed_Protocol" #:doc
                             '(lib "thrift/scribblings/thrift.scrbl")])
               (list "JSON"         "minimal-recommended" 
                     @secref["JSON_Protocol" #:doc
                             '(lib "thrift/scribblings/thrift.scrbl")])
               (list "Compact"      "other-recommended"
                     @secref["Compact_Protocol" #:doc
                             '(lib "thrift/scribblings/thrift.scrbl")])
               (list "Debug"        "other-recommended"
                     @secref["S-Expression_Protocol" #:doc
                             '(lib "thrift/scribblings/thrift.scrbl")]))]

@;{============================================================================}
@section[]{Processor Layer}

The key element of the processor layer is the definition of an interface used
by the server layer to denote a component that reads from one and writes to
another protocol. This interface is described in the Thrift documentation
as follows.

@verbatim|{
  interface TProcessor {
    bool process(TProtocol in, TProtocol out) throws TException
  }
}|

The @racket[thrift/processor/common] module provides the following contract
definition that describes a function in the same manner as the interface
above.

@racketblock[
(define protocol-processor/c
  (-> decoder? encoder? boolean?))
]


@;{============================================================================}
@section[]{Server Layer}

The following table lists the servers defined by Thrift, this is not currently
the focus of this package and none are implemented.


@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Feature} @bold{Coverage}       @bold{Racket})
               (list "SimpleServer" "minimal-required"    "not yet defined")
               (list "Non-Blocking" "other-recommended"   "not yet defined")
               (list "Threaded"     "other-recommended"   "not yet defined")
               (list "Thread Pool"  "other-recommended"   "not yet defined"))]

