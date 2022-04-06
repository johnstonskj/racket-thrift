#lang racket/base
;;
;; thrift - transport/file.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide

 (contract-out
  
  [open-input-file-transport
   (-> string? input-transport?)]

  [open-output-file-transport
   (-> string? output-transport?)]))

;; ---------- Requirements

(require racket/class
         thrift/transport/common
         thrift/transport/exn-common
         thrift/private/logging)

;; ---------- Implementation

(define *transport-name* "file")

(define (open-input-file-transport file-path)
  (open-file-transport file-path 'input))

(define (open-output-file-transport file-path)
  (open-file-transport file-path 'output))

;; ---------- Internal procedures

(define (open-file-transport file-path direction)
  (log-thrift-info "opening thrift file: ~a for ~a" file-path direction)
  (cond
    [(not (file-exists? file-path))
     (raise (file-not-exists (current-continuation-marks)))]
    [(not (member 'read (file-or-directory-permissions file-path)))
     (raise (permission-error-read (current-continuation-marks)))]
    [(not (member 'write (file-or-directory-permissions file-path)))
     (raise (permission-error-write (current-continuation-marks)))]
    [else
     (define port
       (cond
         [(equal? direction 'input)
          (open-input-file file-path #:mode 'binary)]
         [(equal? direction 'output)
          (open-output-file file-path #:mode 'binary #:exists 'can-update)]))
     (file-stream-buffer-mode port 'block)
     
     (make-object positional-transport% *transport-name* file-path port)]))

