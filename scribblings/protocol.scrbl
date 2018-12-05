#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          (for-label racket/base
                     racket/contract
                     thrift))

@;{============================================================================}

@(define example-eval (make-base-eval
                      '(require racket/string
                                thrift)))

@;{============================================================================}

@title[]{Thrift Protocol Support}

Support for Thrift protocol encodings

@;{============================================================================}
@section[]{Protocol Common}
@defmodule[thrift/protocol/common]

@defstruct*[protocol-id
            ([string string?]
             [numeric number?]
             [version number?])]{
TBD
}

@defstruct*[message-header
            ([name string?]
             [type type?]
             [sequence-id integer?])]{
TBD
}

@defstruct*[field-header
            ([name string?]
             [type type?]
             [id exact-nonnegative-integer?])]{
 TBD
}

@defstruct*[map-header
            ([key-type type?]
             [element-type type?]
             [length exact-nonnegative-integer?])]{
TBD
}

@defstruct*[list-or-set
            ([element-type type?]
             [length exact-nonnegative-integer?])]{
TBD
}

@defstruct*[encoder
           ([name string?]
            [message-begin (-> message-header? any/c)]
            [message-end (-> any/c)]
            [struct-begin (-> string? any/c)]
            [struct-end (-> any/c)]
            [field-begin (-> field-header any/c)]
            [field-end (-> any/c)]
            [field-stop (-> any/c)]
            [map-begin (-> map-header? any/c)]
            [map-end (-> any/c)]
            [list-begin (-> list-or-set? any/c)]
            [list-end (-> any/c)]
            [set-begin (-> list-or-set? any/c)]
            [set-end (-> any/c)]
            [boolean (-> boolean? any/c)]
            [byte (-> byte? any/c)]
            [bytes (-> bytes? any/c)]
            [int16 (-> integer? any/c)]
            [int32 (-> integer? any/c)]
            [int64 (-> integer? any/c)]
            [double (-> flonum? any/c)]
            [string (-> string? any/c)])]{
TBD
}

@defstruct*[decoder
           ([name string?]
            [message-begin (-> message-header?)]
            [message-end (-> void?)]
            [struct-begin (-> string?)]
            [struct-end (-> void?)]
            [field-begin (-> field-header?)]
            [field-end (-> void?)]
            [field-stop (-> any/c)]
            [map-begin(-> map-header?)]
            [map-end (-> void?)]
            [list-begin (-> list-or-set?)]
            [list-end (-> void?)]
            [set-begin (-> list-or-set?)]
            [set-end (-> void?)]
            [boolean(-> boolean?)]
            [byte (-> byte?)]
            [bytes (-> bytes?)]
            [int16 (-> integer?)]
            [int32 (-> integer?)]
            [int64 (-> integer?)]
            [double (-> flonum?)]
            [string (-> string?)])]{
TBD
}

@;{============================================================================}
@section[]{Binary Protocol}
@defmodule[thrift/protocol/binary]

This protocol is described by the 
@hyperlink["https://github.com/apache/thrift/blob/master/doc/specs/thrift-binary-protocol.md"]{Binary
Protocol} specification.

@defproc[(make-binary-encoder
          [t  transport?])
         (or/c encoder? #f)]{
 TBD
}

@defproc[(make-binary-decoder
          [t  transport?])
         (or/c decoder? #f)]{
 TBD
}

@;{============================================================================}
@section[]{Compact Protocol}
@defmodule[thrift/protocol/compact]

This protocol is described by the 
@hyperlink["https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md"]{
 Compact Protocol} specification.

@defproc[(make-compact-encoder
          [t  transport?])
         (or/c encoder? #f)]{
 TBD
}

@defproc[(make-compact-decoder
          [t  transport?])
         (or/c decoder? #f)]{
 TBD
}

@;{============================================================================}
@section[]{JSON Protocol}
@defmodule[thrift/protocol/json]

@defproc[(make-json-encoder
          [t  transport?])
         (or/c encoder? #f)]{
 TBD
}

@defproc[(make-json-decoder
          [t  transport?])
         (or/c decoder? #f)]{
 TBD
}

@;{============================================================================}
@section[]{Multiplexed Protocol}
@defmodule[thrift/protocol/multiplexed]

This protocol wraps an existing protocol with the ability to support the calling
of multiple services over the same protocol/transport pair. It is described by the
@hyperlink["https://issues.apache.org/jira/browse/THRIFT-563"]{THRIFT-563} issue.

@defstruct*[(mux-message-header message-header)
            ([service-name string?])]{
TBD
}

@defproc[(make-multiplexed-encoder
          [wrapped encoder?])
         (or/c encoder? #f)]{
 TBD
}

@defproc[(make-multiplexed-decoder
          [wrapped decoder?])
         (or/c decoder? #f)]{
 TBD
}

@defproc[(register-service
          [mux decoder?]
          [service-name string?]
          [processor protocol-processor/c])
         void?]{
TBD
}

@defproc[(deregister-service
          [mux decoder?]
          [service-name string?])
         void?]{
TBD
}


@;{============================================================================}
@section[]{S-Expression Protocol}
@defmodule[thrift/protocol/sexpression]

This protocol is intended as a more Racket friendly debug protocol.

@defproc[(make-sexpression-encoder
          [t  transport?])
         (or/c encoder? #f)]{
 TBD
}

@defproc[(make-sexpression-decoder
          [t  transport?])
         (or/c decoder? #f)]{
 TBD
}

@;{============================================================================}
@section[]{Encoding Support}
@defmodule[thrift/protocol/encoding]

@defproc[(encode-exn
          [e encoder?]
          [exn exn:thrift?]
          [reply-to (or/c message-header? #f)])
         void?]{
TBD
}


@;{============================================================================}
@section[]{Decoding Support}
@defmodule[thrift/protocol/decoding]

@deftogether[(@defproc[(type-bool/decode [d decoder?]) boolean?]
               @defproc[(type-byte/decode [d decoder?]) byte?]
               @defproc[(type-int16/decode [d decoder?]) integer?]
               @defproc[(type-int32/decode [d decoder?]) integer?]
               @defproc[(type-int64/decode [d decoder?]) integer?]
               @defproc[(type-double/decode [d decoder?]) flonum?]
               @defproc[(type-string/decode [d decoder?]) string?]
               @defproc[(type-binary/decode [d decoder?]) bytes?])]{
TBD
}

@deftogether[(@defproc[(type-bool/decode-list [d decoder?]) (listof boolean?)]
               @defproc[(type-byte/decode-list [d decoder?]) (listof byte?)]
               @defproc[(type-int16/decode-list [d decoder?]) (listof integer?)]
               @defproc[(type-int32/decode-list [d decoder?]) (listof integer?)]
               @defproc[(type-int64/decode-list [d decoder?]) (listof integer?)]
               @defproc[(type-double/decode-list [d decoder?]) (listof flonum?)]
               @defproc[(type-string/decode-list [d decoder?]) (listof string?)]
               @defproc[(type-binary/decode-list [d decoder?]) (listof bytes?)])]{
TBD
}


@defproc[(decode-a-list
          [d decoder?]
          [element-decoder procedure?])
         list?]{
TBD
}

@defproc[(decode-a-union
          [d decoder?]
          [constructor procedure?]
          [struct-schema (hash/c exact-nonnegative-integer? thrift-field?)])
         struct?]{
TBD
}

@defproc[(decode-a-struct
          [d decoder?]
          [struct-schema (hash/c exact-nonnegative-integer? thrift-field?)])
         struct?]{
TBD
}
