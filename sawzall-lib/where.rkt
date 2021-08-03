#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract/base
         racket/match
         racket/set
         syntax/parse/define
         "grouped-df.rkt"
         "grouping.rkt"
         "syntax.rkt")
(provide where where* deduplicate
         where/int)

(define-syntax (where stx)
  (row-syntax-form stx #'where/int))

(define (where/int df proc)
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

(define-syntax-parse-rule (where* df (name:id ...) (pat:expr ...))
  #:declare df (expr/c #'(or/c data-frame? grouped-data-frame?))
  #:fail-when (not (= (length (attribute name)) (length (attribute pat))))
  "number of names must be the same as the number of match patterns"
  (where df.c (name ...)
         (match (list name ...)
           [(list pat ...) #t]
           [_ #f])))

(define-syntax-parse-rule (deduplicate df fld:id ...+)
  #:declare df (expr/c #'(or/c data-frame? grouped-data-frame?))
  (grouped-df-apply
   (λ (sub-df)
     (define seen-set (mutable-set))
     (define (seen? v) (set-member? seen-set v))
     (define (add-seen v) (set-add! seen-set v))
     (define (filter-seen v)
       (cond
         [(seen? v) #f]
         [else (add-seen v) #t]))
     (where/int sub-df (row-proc (list (symbol->string 'fld) ...)
                                 (λ (fld ...) (filter-seen (list fld ...))))))
   df.c))
