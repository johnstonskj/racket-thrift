#lang racket/base
;;
;; thrift - bytedebug.
;;   Support for Thrift encoding
;;
;; ~ Simon Johnston 2018.
;;

(require racket/contract)

(provide

 (contract-out

  [~byte
   (-> byte? string?)]

  [~bytes
   (-> bytes? string?)]

  [~bytes/log
   (->* (bytes? (or symbol? string?)) (logger? symbol?) void?)])
 
 hex-display-numeric-format

 hex-display-char-format

 hex-display-line-width)

(require racket/bool
         racket/format
         racket/list
         racket/string)

(define hex-display-numeric-format
  (make-parameter 'hex
                  (λ (nf) (if (member nf '(hex octal binary))
                              nf
                              (hex-display-numeric-format)))))

(define hex-display-char-format
  (make-parameter 'racket
                  (λ (cf) (if (member cf '(none racket))
                              cf
                              (hex-display-char-format)))))

(define hex-display-line-width
  (make-parameter 16
                  (λ (lw) (if (even? lw)
                              lw
                              (sub1 lw)))))

(define hex-display-prefix-width 8)

(define hex-display-char-width 4)

(define hex-display-binary-width 8)

(define hex-display-space " ")

(define hex-display-zero "0")

(define hex-display-center "-")

(define format-string
  (hash 'hex "~x"
        'octal "~O"
        'binary "~b"))

(define (~byte v)
  (define c (integer->char v))
  (define-values (formatted pad-char)
    (cond
      [(and (char-graphic? c) (symbol=? (hex-display-char-format) 'racket))
       (values (format "~c" c)
               hex-display-space)]
      [else
       (values (format (hash-ref format-string (hex-display-numeric-format)) v)
               hex-display-zero)]))
  (string-append (~a formatted
                     #:width (if (symbol=? (hex-display-numeric-format) 'binary)
                                 hex-display-binary-width
                                 hex-display-char-width)
                     #:align 'right
                     #:left-pad-string pad-char)
                 hex-display-space))

(define (~bytes bs)
  (let ([out (open-output-string)])
    (for ([b (bytes->list bs)] [idx (range (bytes-length bs))])
      (cond
        [(= (remainder idx (hex-display-line-width)) 0)
         (when (> idx 0)
           (newline out))
         (display
          (~a (format "~x" idx)
              #:width hex-display-prefix-width
              #:align 'right
              #:left-pad-string hex-display-zero) out)
         (display hex-display-space out)]
        [(= (remainder idx (/ (hex-display-line-width) 2)) 0)
         (display
          (format "~a~a~a" hex-display-space hex-display-center hex-display-space) out)])
      (display (~byte b) out))
    (newline out)
    (get-output-string out)))

(define (~bytes/log bs prefix [logger (current-logger)] [level 'debug])
  (define output (~bytes bs))
  (for ([line (string-split output (string #\newline))])
    (log-message logger level (format "~a: ~a" prefix line) (current-continuation-marks))))

(module+ test
  (require rackunit racket/logging)

  (define test-cases
    (for*/list ([num '(hex octal binary)] [char '(none racket)] [width '(16)])
      (list num char width)))
  
  (define test-data
    (bytes-append #"\202!*\4test\5hello\5world\0simon0\3\0\5first\312\1\6second\314\1\5third\316\1"))

  (for ([params test-cases])
    (define description
      (format "~~bytes test; number format: ~a, character format: ~a, width: ~a"
              (first params) (second params) (third params)))
    (displayln description)
    (test-case
     description
     (parameterize ([hex-display-numeric-format (first params)]
                    [hex-display-char-format (second params)]
                    [hex-display-line-width (third params)])
       (display (~bytes test-data)))))

  (with-logging-to-port (current-output-port)
    (λ ()
      (~bytes/log test-data 'test-data (current-logger) 'warning))
    'debug))
