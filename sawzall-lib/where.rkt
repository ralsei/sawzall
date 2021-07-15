#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract
         racket/match
         "grouped-df.rkt"
         "grouping.rkt"
         "syntax.rkt")
(provide where)

(define-syntax (where stx)
  (row-syntax-form stx #'where/int))

(define/contract (where/int df proc)
  (-> (or/c data-frame? grouped-data-frame?) row-proc? (or/c data-frame? grouped-data-frame?))
  (grouped-df-apply (where-df _ proc) df))

(define (where-df df proc)
  (define internal-df (sub-data-frame-delegate-frame df))
  (match-define (row-proc binder f?) proc)
  
  (define return-df (make-data-frame))
  (define indices
    (for*/list ([(vs idx) (in-indexed (apply in-data-frame/list/sub df binder))]
                #:when (apply f? vs))
      idx))
  (for ([name (in-list (df-series-names internal-df))])
    (df-add-series! return-df
                    (make-series name #:data (for/vector ([idx (in-list indices)])
                                               (df-ref/sub df idx name)))))
  return-df)
