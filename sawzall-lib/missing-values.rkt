#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/function
         racket/sequence
         racket/set
         syntax/parse/define
         "helpers.rkt"
         "grouped-df.rkt"
         "grouping.rkt"
         "slice-spec.rkt"
         "syntax.rkt"
         "where.rkt")
(provide drop-na replace-na)

; replaces all occurrences of a in vec with b
(define (vector-replace vec a b)
  (for/vector ([val (in-vector vec)])
    (if (equal? val a)
        b
        val)))

(define (replace-na df . args)
  (ignore-groups-apply (replace-na-df _ args) df))

(define (replace-na-df df args)
  (when (not (even? (length args)))
    (error 'replace-na "column specified with nothing to replace to"))

  (define return-df (df-shallow-copy df))
  (for ([clause (in-slice 2 (in-list args))])
    (define col (car clause))
    (define to (cadr clause))

    (df-del-series! return-df col)
    (df-add-series! return-df
                    (make-series col #:data (vector-replace (df-select df col)
                                                            (df-na-value df col) to))))
  return-df)

(define-syntax-parse-rule (drop-na df spec:slice-spec)
  #:declare df (expr/c #'(or/c data-frame? grouped-data-frame?))
  (ignore-groups-apply (λ (df) (drop-na-df df spec.parsed)) df))

(define (drop-na-df df parsed-spec)
  (define columns (set->list (exec-spec-on-df df parsed-spec)))
  (where/int df (row-proc (set->list columns) (λ vals (andmap identity vals)))))
