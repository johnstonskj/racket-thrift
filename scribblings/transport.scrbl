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

@title[]{Thrift Transport Support.}

Support for Thrift transports

@examples[ #:eval example-eval
(require thrift)
; add more here.
]

@;{============================================================================}
@section[]{Transport Common}
@defmodule[thrift/transport/common]

@defstruct*[transport
            ([name string?]
             [source string?]
             [port port?]
             [overrides hash?])]{
TBD
}

@defproc[(transport-peek
          [t transport?])
         byte?]{
TBD
}
  
@defproc[(transport-read-byte
          [t transport?])
         byte?]{
TBD
}
  
@defproc[(transport-read-bytes
          [t transport?]
          [amt exact-positive-integer?])
         bytes?]{
TBD
}
  
@defproc[(transport-read
          [t transport?])
         any/c]{
TBD
}

@defproc[(transport-write-byte
          [t transport?]
          [b byte?])
         void?]{
TBD
}
  
@defproc[(transport-write-bytes
          [t transport?]
          [bs bytes?]
          [start-pos exact-nonnegative-integer? 0]
          [end-pos exact-nonnegative-integer?])
         void?]{
TBD
}
  
@defproc[(transport-write
          [t transport?]
          [v any/c])
         void?]{
TBD
}
  
@defproc[(transport-size
          [t transport?])
         (or/c exact-nonnegative-integer? eof-object?)]{
TBD
}

@defproc*[([(transport-read-position
             [t transport?])
            (or/c exact-nonnegative-integer? eof-object?)]
           [(transport-read-position
             [t transport?]
             [pos exact-nonnegative-integer?])
            (or/c exact-nonnegative-integer? eof-object?)])]{
TBD
}

@defproc[(input-transport?
          [t transport?])
         boolean?]{
TBD
}
  
@defproc[(output-transport?
          [t transport?])
         boolean?]{
TBD
}

@defproc[(flush-transport
          [t output-transport?])
         void?]{
TBD
}

@defproc[(close-transport
          [t transport?])
         any/c]{
TBD
}

@;{============================================================================}
@section[]{Buffered and Framed Transports}
@defmodule[thrift/transport/buffered]

@defproc[(open-input-buffered-transport
          [wrapped input-transport?])
         input-transport?]{
TBD
}

@defproc[(open-output-buffered-transport
          [wrapped output-transport?])
         output-transport?]{
TBD
}

@defproc*[(
  [(buffered-read-length) positive-integer?]
  [(buffered-read-length [length positive-integer?]) void?]
)]{
TBD
}

@defproc[(open-input-framed-transport
          [wrapped input-transport?])
         input-transport?]{
TBD
}

@defproc[(open-output-framed-transport
          [wrapped output-transport?])
         output-transport?]{
TBD
}

@;{============================================================================}
@section[]{File Transport}
@defmodule[thrift/transport/file]

@defproc[(open-input-file-transport
          [file-path string?])
         transport?]{
TBD
}

@defproc[(open-output-file-transport
          [file-path string?])
         transport?]{
TBD
}

@;{============================================================================}
@section[]{Console Transport}
@defmodule[thrift/transport/console]

@defproc[(open-input-console-transport
          [buffer bytes?])
         transport?]{
TBD
}

@defproc[(open-output-console-transport)
         transport?]{
TBD
}

@;{============================================================================}
@section[]{In-Memory Transport}
@defmodule[thrift/transport/memory]

@defproc[(open-input-memory-transport
          [read-buffer bytes?])
         transport?]{
TBD
}

@defproc[(open-output-memory-transport)
         transport?]{
TBD
}

@defproc[(transport-output-bytes
          [t transport?])
         bytes?]{
TBD
}

