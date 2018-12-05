# Racket package thrift

[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-thrift.svg?style=flat-square)](https://github.com/johnstonskj/racket-thrift/releases)
[![Travis Status](https://travis-ci.org/johnstonskj/racket-thrift.svg)](https://www.travis-ci.org/johnstonskj/racket-thrift)
[![Coverage Status](https://coveralls.io/repos/github/johnstonskjracket-/thrift/badge.svg?branch=master)](https://coveralls.io/github/johnstonskj/racket-thrift?branch=master)
[![raco pkg install thrift](https://img.shields.io/badge/raco%20pkg%20install-thrift-blue.svg)](http://pkgs.racket-lang.org/package/thrift)
[![Documentation](https://img.shields.io/badge/raco%20docs-rml--core-blue.svg)](http://docs.racket-lang.org/thrift/index.html)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-thrift.svg)](https://github.com/johnstonskj/racket-thrift/stargazers)
![mit License](https://img.shields.io/badge/license-mit-118811.svg)

This package provides an implementation of basic read (write coming eventually) capabilities for Apache Parquet files. Parquet is a commonly used format in cloud-native systems, the Hadoop ecosystem and machine learning applications.

[![Apache Parquet](https://raw.githubusercontent.com/johnstonskj/racket-thrift/master/thrift/scribblings/thrift_logo.png)](https://thrift.apache.org)

## Modules

* `thrift` - Common type definitions for the Thrift stack.
* `thrift/idl/generator` - Generate Racket modules based upon an IDL.
* `thrift/idl/language` - Racket syntax for defining IDLs in Racket.
* `thrift/protocol/plain` - The plain binary protocol.
* `thrift/protocol/compact` - The compact binary protocol.
* `thrift/transport/file` - A File transport.

## Command Line Launchers

* `rthrift` - Used to generate Racket modules from Thrift IDL.

## Example

```scheme
(require thrift/file
         thrift/generated/thrift
         thrift/transport/common)

(define tport (open-input-thrift-file "../test-data/nation.impala.thrift"))
(define metadata (read-metadata tport))

(displayln (format "File Metadata: ~a, Version: ~a, Num Rows: ~a"
                   (transport-source tport)
                   (file-metadata-version metadata)
                   (file-metadata-num-rows metadata)))

(close-thrift-file tport)
```

## Installation

* To install (from within the package directory): `raco pkg install`
* To install (once uploaded to [pkgs.racket-lang.org](https://pkgs.racket-lang.org/)): `raco pkg install thrift`
* To uninstall: `raco pkg remove thrift`
* To view documentation: `raco docs thrift`

## History

* **1.0** - Initial Stable Version
  * Thrift IDL and generator working for types and decoding functions.
  * Thrift compact protocol working for read.
  * Thrift file transport working for read.
  * Parquet file reader and launcher working for metadata only.
* **0.1** - Initial (Unstable) Version

[![Racket Language](https://raw.githubusercontent.com/johnstonskj/racket-scaffold/master/scaffold/plank-files/racket-lang.png)](https://racket-lang.org/)
