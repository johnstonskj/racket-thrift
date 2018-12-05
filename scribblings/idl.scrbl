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

@title[]{Thrift IDL Support.}

Support for Thrift format definitions

@examples[ #:eval example-eval
(require thrift
         thrift/idl/language)

(define-thrift-namespace parquet)

(define-thrift-enum
  parquet-type 0
  (boolean
   int32
   int64
   int96 ; deprecated
   float
   double
   byte-array
   fixed-len-byte-arrary))

(define-thrift-struct file-metadata
  ([1 version required type-int32]
   [2 schema required list-of schema-element]
   [3 num-rows required type-int64]
   [4 row-groups required list-of row-group]
   [5 key-value-metadata optional list-of key-value]
   [6 created-by optional type-string]
   [7 column-orders optional list-of column-order]))
]

@;{============================================================================}
@section[]{Type System}
@defmodule[thrift/idl/common]

@subsection[]{Core Types}

@defproc[(type? [v any/c]) boolean?]

@deftogether[(@defthing[type-stop type?]
               @defthing[type-void type?]
               @defthing[type-bool type?]
               @defthing[type-byte type?]
               @defthing[type-int8 type?]
               @defthing[type-double type?]
               @defthing[type-int16 type?]
               @defthing[type-int32 type?]
               @defthing[type-int64 type?]
               @defthing[type-string type?]
               @defthing[type-binary type?]
               @defthing[type-utf-7 type?]
               @defthing[type-struct type?]
               @defthing[type-map type?]
               @defthing[type-list type?]
               @defthing[type-set type?]
               @defthing[type-utf-8 type?]
               @defthing[type-utf-16 type?])]{
TBD
}

@deftogether[(@defproc[(integer->type [n exact-nonnegative-integer?]) type?]
              @defproc[(type/decode [d decoder?]) type?]
              @defproc[(type/names) (listof string?)])]{
TBD
}

@subsection[]{Field Type Information}

@defproc[(required-type? [v any/c]) boolean?]

@deftogether[(@defthing[required-type-required required-type?]
               @defthing[required-type-optional required-type?]
               @defthing[required-type-default required-type?])]{
TBD
}

@deftogether[(@defproc[(integer->required-type [n exact-nonnegative-integer?]) required-type?]
              @defproc[(required-type/decode [d decoder?]) required-type?]
              @defproc[(required-type/names) (listof string?)])]{
TBD
}

@defproc[(container-type? [v any/c]) boolean?]

@deftogether[(@defthing[container-type-list-of container-type?]
               @defthing[container-type-set-of container-type?]
               @defthing[container-type-map-of container-type?]
               @defthing[container-type-none container-type?])]{
TBD
}

@deftogether[(@defproc[(integer->container-type [n exact-nonnegative-integer?]) container-type?]
              @defproc[(container-type/decode [d decoder?]) container-type?]
              @defproc[(container-type/names) (listof string?)])]{
TBD
}


@defstruct*[thrift-field
            ([id identifier?]
             [name string?]
             [required symbol?]
             [container symbol?]
             [major-type symbol?]
             [minor-type symbol?]
             [position exact-nonnegative-integer?])
            #:mutable]{
TBD
}

@;{============================================================================}
@section[]{IDL Language}
@defmodule[thrift/idl/language]

@defform[(define-thrift-namespace namespace)
         #:contracts ([namespace string?])]{
TBD
}

@defform[(define-thrift-enum id maybe-start value ...)
         #:grammar
         [(id string?)
          (maybe-start (code:line)
                       exact-nonnegative-integer?)
          (value-expr identifier?
                      [identifier? exact-nonnegative-integer?])
 ]]{
TBD
}

@defform[#:literals (map-of)
         (define-thrift-struct [id string?] field ...)
         #:grammar
         [(field (index name maybe-req maybe-con elem-type)
                 (index name maybe-req map-of elem-type key-type))
          (maybe-required (code:line)
                          required-type?)
          (maybe-container (code:line)
                           container-type?)]
         #:contracts ([index exact-nonnegative-integer?]
                      [name identifier?]
                      [elem-type identifier?]
                      [key-type identifier?])]{
TBD
}

@;{============================================================================}
@section[]{Code Generator}
@defmodule[thrift/idl/generator]

@defproc[(process-file
          [file-path string? "."]
          [module-prefix string? ""]
          [over-write? boolean? #f])
         void?]{
TBD
}

@subsection[]{Command-Line Launcher}

The @racket[parquet/idl/generator] module is also used to create a launcher, @code{rthrift}
that wraps @racket[process-file] to generate bindings for an IDL file from a Racket
description.

@verbatim|{
rthrift [ <option> ... ] <file-path>
 where <option> is one of
  -v, --verbose : Compile with verbose messages
  -V, --very-verbose : Compile with very verbose messages
  -o <path>, --output-path <path> : Directory to write the output into
  -m <module>, --module-prefix <module> : Prefix generated modules with a module path
  -f, --force-overwrite : Over-write any existing files
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
}|

To generate the files used in the module @racket[parquet], we use the following
command.

@code{$ rthrift -o parquet/generated -m parquet/generated parquet/format.rkt}

This generates the following files, in the @code{parquet/generated} directory.

@itemlist[
  @item{@code{parquet.rkt} - The core types, expanded, used by the following.}
  @item{@code{parquet-encode.rkt} - All the encoder functions for the types above.}
  @item{@code{parquet-decode.rkt} - All the decoder functions for the types above.}
  @item{@code{parquet.scrbl} - Documentation for the three modules above.}
]

