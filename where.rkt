#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract
         racket/match
         "grouping.rkt"
         "syntax.rkt")
(provide where)

(define-syntax (where stx)
  (row-syntax-form stx #'where/int))

(define/contract (where/int df proc)
  (-> (or/c data-frame? grouped-data-frame?) row-proc? (or/c data-frame? grouped-data-frame?))
  (group-map (where-df _ proc) df))

(define (where-df df proc)
  (match-define (row-proc binder f?) proc)
  
  (define return-df (make-data-frame))
  (define indices
    (for*/list ([(vs idx) (in-indexed (apply in-data-frame/list df binder))]
                #:when (apply f? vs))
      idx))
  (for ([name (in-list (df-series-names df))])
    (df-add-series! return-df
                    (make-series name #:data (for/vector ([idx (in-list indices)])
                                               (df-ref df idx name)))))
  return-df)
