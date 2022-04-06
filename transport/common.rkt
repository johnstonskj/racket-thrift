#lang racket/base
;;
;; thrift - transport/common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(require racket/contract)

(provide
 
 transport%

 positional-transport%

 transport?

 positional-transport?

 input-transport?

 output-transport?)

;; ---------- Requirements

(require (prefix-in base: racket/base)
         racket/bool
         racket/class
         racket/struct
         thrift/transport/exn-common)

;; ---------- Implementation (Classes)

(define transport%

  (class object%

    (init-field name source port)

    (super-new)

    (define/public (read-byte)
      (check-input-port port)
      (base:read-byte port))

    (define/public (read-bytes count)
      (check-input-port port)
      (base:read-bytes count port))

    (define/public (read)
      (check-input-port port)
      (base:read port))

    (define/public (peek)
      (check-input-port port)
      (base:peek-byte port))
    
    (define/public (available?)
      (check-input-port port)
      (base:byte-ready? port))
    
    (define/public (write-byte b)
      (check-output-port port)
      (base:write-byte b port))

    (define/public (write-bytes bs [start 0] [end #f])
      (check-output-port port)
      (if (false? end)
          (base:write-bytes bs port start)
          (base:write-bytes bs port start end)))

    (define/public (write v)
      (check-output-port port)
      (base:write v port))

    (define/public (flush)
      (check-output-port port)
      (base:flush-output port))
    
    (define/public (close)
      (cond
        [(input-port? port)
         (base:close-input-port port)]
        [(port? port)
         (send this flush)
         (base:close-output-port port)]
        [else (error "not a port?")]))
    
    (define/public (input?)
      (base:input-port? port))

    (define/public (closed?)
      (base:port-closed? port))))


(define positional-transport%

  (class transport%

    (super-new)

    (define/public (length)
      (cond
        [(file-stream-port? (get-field port this))
         (file-size (get-field source this))]
        [else
         #f]))

    (define/public (position)
      (file-position (get-field port this)))

    (define/public (set-position! pos)
      (file-position (get-field port this) pos))))

;; ---------- Implementation (Procedures)

(define (transport? v)
  (is-a? v transport%))
  
(define (positional-transport? v)
  (is-a? v positional-transport%))
  
(define (input-transport? v)
  (and (is-a? v transport%)
       (send v input?)))
  
(define (output-transport? v)
  (and (is-a? v transport%)
       (not (send v input?))))
  
;; ---------- Internal procedures

(define (check-input-port port)
  (unless (and (input-port? port)
               (not (port-closed? port)))
    (raise (not-open-for-input (current-continuation-marks)))))

(define (check-output-port port)
  (unless (and (port? port)
               (not (input-port? port))
               (not (port-closed? port)))
    (raise (not-open-for-output (current-continuation-marks)))))

;; ---------- Internal tests

(module+ test
  (require rackunit)
  (define out-port (open-output-bytes))
  (define out-transport (make-object transport% "test" 'test-write out-port))
  (check-equal? (send out-transport input?) #f)
  (check-equal? (send out-transport closed?) #f)
  (send out-transport write-byte 2)
  (send out-transport write-byte 4)
  (send out-transport write-byte 8)
  (check-equal? (send out-transport write-bytes #"ABCDEF") 6)
  (send out-transport write "hello world")
  (send out-transport flush)
  (send out-transport close)
  (check-equal? (send out-transport closed?) #t)
  (define in-port (open-input-bytes (get-output-bytes out-port)))
  (define in-transport (make-object transport% "test" 'test-read in-port))
  (check-equal? (send in-transport input?) #t)
  (check-equal? (send in-transport closed?) #f)
  (check-equal? (send in-transport read-byte) 2)
  (check-equal? (send in-transport read-byte) 4)
  (check-equal? (send in-transport read-byte) 8)
  (check-equal? (send in-transport read-bytes 6) #"ABCDEF")
  (check-equal? (send in-transport read) "hello world")
  (send in-transport close)
  (check-equal? (send in-transport closed?) #t))