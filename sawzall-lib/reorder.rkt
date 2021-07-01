#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract
         racket/match
         racket/vector
         "grouping.rkt"
         "syntax.rkt")
(provide (contract-out [by-vector (-> vector? (-> any/c any/c boolean?))])
         reorder)

; reorders a vector based on the given indices
; example:
;   (vector-reorder (vector 1 2 3) (vector 2 1 0))
;   => (vector 3 2 1)
(define (vector-reorder vec indices)
  (when (not (= (vector-length indices) (vector-length vec)))
    (error 'vector-reorder "index list not same length as vector"))
  (for/vector ([idx (in-vector indices)])
    (vector-ref vec idx)))

(define-syntax (reorder stx)
  (sort-syntax-form stx #'reorder/int))

(define/contract (reorder/int df proc)
  (-> (or/c data-frame? grouped-data-frame?) sort-proc? (or/c data-frame? grouped-data-frame?))
  (ignore-grouping (reorder-df _ proc) df))

(define (reorder-df df proc)
  (match-define (sort-proc cols comparators) proc)

  (for/fold ([df df])
            ([col (in-list cols)]
             [cmp (in-list comparators)])
    (reorder-once df col cmp)))

(define (reorder-once df col cmp?)
  (define return-df (make-data-frame))
  (define col-data (df-select df col))
  (define indices
    (vector-sort (build-vector (vector-length col-data) (λ (x) x))
                 cmp? #:key (vector-ref col-data _)))

  (for ([col (in-list (df-series-names df))])
    (df-add-series! return-df
                    (make-series col #:data (vector-reorder (df-select df col) indices))))
  return-df)

(define (by-vector vec)
  (define hsh
    (for/hash ([(v idx) (in-indexed (in-vector vec))])
      (values v idx)))
  (λ (a b)
    (< (hash-ref hsh a) (hash-ref hsh b))))
