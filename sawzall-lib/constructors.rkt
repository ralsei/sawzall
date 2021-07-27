#lang racket/base
(require (for-syntax racket/base
                     racket/sequence)
         data-frame
         syntax/parse/define)
(provide column-df row-df)

(begin-for-syntax
  (define-syntax-class column-spec
    [pattern col:id
             #:with name #'(symbol->string 'col)]
    [pattern col
             #:declare col (expr/c #'string?)
             #:with name #'col.c])

  (define (slice n lst)
    (sequence->list (in-slice n (in-list lst))))

  (define (syntax-vector . args)
    #`(vector #,@args)))

(define-syntax-parse-rule (column-df [col:column-spec col-data:expr] ...)
  (let ()
    (define df (make-data-frame))
    (df-add-series! df (make-series col.name #:data col-data)) ...
    df))

(define-syntax-parse-rule (row-df [col:column-spec ...] value:expr ...)
  #:with (column-vec ...)
         (apply map syntax-vector
                (slice (length (attribute col.name)) (attribute value)))
  (let ()
    (column-df [col.name column-vec] ...)))
