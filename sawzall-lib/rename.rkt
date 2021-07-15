#lang racket/base
(require data-frame
         fancy-app
         racket/sequence
         "grouped-df.rkt"
         "grouping.rkt")
(provide rename)

(define (rename df . args)
  (grouped-df-apply (rename-df _ args) df))

(define (rename-df df args)
  (when (not (even? (length args)))
    (error 'rename "column specified with nothing to rename to"))

  (define return-df (df-dumb-copy/sub df))
  (for ([rename-clause (in-slice 2 (in-list args))])
    (define from (car rename-clause))
    (define to (cadr rename-clause))

    (df-add-derived! return-df to (list from) (λ (x) (car x)))
    (df-del-series! return-df from))
  return-df)
