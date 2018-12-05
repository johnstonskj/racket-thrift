#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          racket/contract
          thrift
          (for-label racket/base
                     racket/contract
                     thrift))

@;{============================================================================}

@(define example-eval (make-base-eval
                      '(require racket/string
                                racket/contract
                                thrift)))

@;{============================================================================}

@title[]{Thrift Processor Support}

Support for Thrift processor contracts.

@examples[ #:eval example-eval
(require thrift)

(define/contract (my-processor in out)
  transport-processor/c
  #f)
]

@;{============================================================================}
@section[]{Processor Types}
@defmodule[thrift/processor/common]

@defthing[transport-processor/c
          flat-contract?]{
TBD @racket[(-> input-transport? output-transport? boolean?)]
}

@defthing[protocol-processor/c
          flat-contract?]{
TBD @racket[(-> decoder? encoder? boolean?)]
}
