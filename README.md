# Racket package thrift

[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-thrift.svg?style=flat-square)](https://github.com/johnstonskj/racket-thrift/releases)
[![Travis Status](https://travis-ci.org/johnstonskj/racket-thrift.svg)](https://www.travis-ci.org/johnstonskj/racket-thrift)
[![Coverage Status](https://coveralls.io/repos/github/johnstonskjracket-/thrift/badge.svg?branch=master)](https://coveralls.io/github/johnstonskj/racket-thrift?branch=master)
[![raco pkg install thrift](https://img.shields.io/badge/raco%20pkg%20install-thrift-blue.svg)](http://pkgs.racket-lang.org/package/thrift)
[![Documentation](https://img.shields.io/badge/raco%20docs-rml--core-blue.svg)](http://docs.racket-lang.org/thrift/index.html)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-thrift.svg)](https://github.com/johnstonskj/racket-thrift/stargazers)
![mit License](https://img.shields.io/badge/license-mit-118811.svg)

This package provides an implementation of the core for Apache Thrift. While not a complete implementation of all the required and recommended capabilities it is reasonably complete.

## Modules

* `thrift` - Common type definitions for the Thrift stack.
* `thrift/idl/*` - A Racket syntax for defining Thrift IDL in-line, and to generate Racket modules based upon an IDL.
* `thrift/processor/*` - Implementations for common processor types..
* `thrift/protocol/*` - Implementations for core protocol types.
* `thrift/transport/*` - Implementations for core protocol types.

## Command Line Launchers

* `rthrift` - Used to generate Racket modules from Thrift IDL.

## Example

```racket
(require thrift/protocol/compact
         thrift/transport/memory)

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

(write (transport-output-bytes t))
```

## Installation

* To install (from within the package directory): `raco pkg install`
* To install (once uploaded to [pkgs.racket-lang.org](https://pkgs.racket-lang.org/)): `raco pkg install thrift`
* To uninstall: `raco pkg remove thrift`
* To view documentation: `raco docs thrift`

## History

* **1.0** - Initial Stable Version

[![Racket Language](https://raw.githubusercontent.com/johnstonskj/racket-scaffold/master/scaffold/plank-files/racket-lang.png)](https://racket-lang.org/)
