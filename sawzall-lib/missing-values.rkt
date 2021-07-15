#lang racket/base
(require data-frame
         fancy-app
         racket/sequence
         "helpers.rkt"
         "grouped-df.rkt"
         "grouping.rkt")
(provide replace-na)

; replaces all occurrences of a in vec with b
(define (vector-replace vec a b)
  (for/vector ([val (in-vector vec)])
    (if (equal? val a)
        b
        val)))

(define (replace-na df . args)
  (grouped-df-apply (replace-na-df _ args) df))

(define (replace-na-df df args)
  (when (not (even? (length args)))
    (error 'replace-na "column specified with nothing to replace to"))

  (define internal-df (sub-data-frame-delegate-frame df))

  (define return-df (df-dumb-copy/sub df))
  (for ([clause (in-slice 2 (in-list args))])
    (define col (car clause))
    (define to (cadr clause))

    (df-del-series! return-df col)
    (df-add-series! return-df
                    (make-series col #:data (vector-replace (df-select/sub df col)
                                                            (df-na-value internal-df col) to))))
  return-df)
