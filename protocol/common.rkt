#lang racket/base
;;
;; thrift - protocol/common.
;;   Support for Thrift encoding
;;
;; Copyright (c) 2018 Simon Johnston (johnstonskj@gmail.com).

(provide
 (struct-out protocol-id)
 (struct-out message-header)
 (struct-out field-header)
 (struct-out map-header)
 (struct-out list-header)

 protocol%

 protocol?)

;; ---------- Requirements

(require racket/class
         thrift/private/protocol-class)

;; ---------- Implementation (Types)

(struct protocol-id
  (string
   numeric
   version) #:prefab)

(struct message-header
  (name type sequence-id) #:prefab)

(struct field-header
  (name type id) #:prefab)

(struct map-header
  (key-type element-type length) #:prefab)

(struct list-header
  (element-type length) #:prefab)

;; ---------- Implementation (Classes)

(define protocol%
  
  (class object%

    (super-new)

    (init-field identity
                input-transport
                output-transport
                [state '()])

    (define/public-final (name)
      (protocol-id-string identity))

    ;; Write API
    (define-write (message-begin message))
    
    (define-write (message-end)
      (send output-transport flush))
    
    (define-write (struct-begin name))
 
    (define-write (struct-end))
    
    (define-write (field-begin field))
 
    (define-write (field-end))
    
    (define-write (field-stop))
    
    (define-write (map-begin map))
 
    (define-write (map-end))
    
    (define-write (list-begin list))
 
    (define-write (list-end))
    
    (define-write (set-begin set))
 
    (define-write (set-end))
    
    (define-write (boolean value))
 
    (define-write (byte value))
 
    (define-write (bytes value))
 
    (define-write (int16 value))
 
    (define-write (int32 value))
 
    (define-write (int64 value))
 
    (define-write (double value))
 
    (define-write (string value))
 
    ;; Read API
    (define-read (message-begin message-header? #f))
    
    (define-read (message-end void? (void)))
    
    (define-read (struct-begin string? ""))
 
    (define-read (struct-end void? (void)))
    
    (define-read (field-begin field-header? #f))
 
    (define-read (field-end void? (void)))
    
    (define-read (field-stop void? (void)))
    
    (define-read (map-begin map-header? #f))
 
    (define-read (map-end void? (void)))
    
    (define-read (list-begin list-header? #f))
 
    (define-read (list-end void? (void)))
    
    (define-read (set-begin list-header? #f))
 
    (define-read (set-end void? (void)))
    
    (define-read (boolean boolean? #f))
 
    (define-read (byte byte? 0))
 
    (define-read (bytes bytes? #""))
 
    (define-read (int16 integer? 0))
 
    (define-read (int32 integer? 0))
 
    (define-read (int64 integer? 0))
 
    (define-read (double flonum? 0.0))
 
    (define-read (string string? ""))))

;; ---------- Implementation (Procedures)

(define (protocol? v)
  (is-a? v protocol%))