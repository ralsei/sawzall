#lang racket/base
(require (for-syntax racket/base)
         data-frame
         racket/sequence
         syntax/parse/define)
(provide column-df row-df)

(begin-for-syntax
  (define-syntax-class column-spec
    [pattern col:id
             #:with name #'(symbol->string 'col)]
    [pattern col
             #:declare col (expr/c #'string?)
             #:with name #'col]))

(define (slice n lst)
  (sequence->list (in-slice n (in-list lst))))

(define-syntax-parse-rule (column-df [col:column-spec col-data:expr] ...)
  (let ()
    (define df (make-data-frame))
    (df-add-series! df (make-series col.name #:data col-data)) ...
    df))

(define-syntax-parse-rule (row-df [col-name:id ...] value:expr ...)
  (let ()
    (define col-strs (list (symbol->string 'col-name) ...))
    (define col-data (apply map vector (slice (length col-strs) (list value ...))))

    (define df (make-data-frame))
    (for ([n (in-list col-strs)]
          [v (in-list col-data)])
      (df-add-series! df (make-series n #:data v)))
    df))
