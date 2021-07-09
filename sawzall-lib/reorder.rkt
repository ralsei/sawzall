#lang racket/base
(require data-frame
         fancy-app
         racket/contract
         racket/vector
         "helpers.rkt"
         "grouping.rkt")
(provide (contract-out [by-vector (-> vector? (-> any/c any/c boolean?))]
                       [reorder (->* ((or/c data-frame? grouped-data-frame?))
                                     (#:in-groups? boolean?)
                                     #:rest (non-empty-listof
                                             (or/c string?
                                                   (cons/c string? (-> any/c any/c boolean?))))
                                     (or/c data-frame? grouped-data-frame?))]))

(define (reorder df #:in-groups? [in-groups? #f] . to-sort)
  (define pairs
    (for/list ([v (in-list to-sort)])
      (if (pair? v)
          v
          (cons v orderable<?))))
  ((if in-groups? group-map ignore-grouping)
   (reorder-df _ pairs) df))

(define (reorder-df df pairs)
  (for/fold ([d df])
            ([p (in-list pairs)])
    (reorder-once d (car p) (cdr p))))

; TODO: figure out df-set-sorted! here, for optimization's sake
(define (reorder-once df col cmp?)
  (define return-df (make-data-frame))
  (define col-data (df-select df col))
  (define indices
    (vector-sort (build-vector (vector-length col-data) (λ (x) x))
                 cmp? #:key (vector-ref col-data _)))

  (for ([col (in-list (df-series-names df))])
    (df-add-series! return-df
                    (make-series col #:data (vector-reorder (df-select df col) indices))))
  (when (not (df-has-na? return-df col))
    (df-set-sorted! return-df col (λ (a b) (or (equal? a b) (cmp? a b)))))
  return-df)

(define (by-vector vec)
  (define hsh
    (for/hash ([(v idx) (in-indexed (in-vector vec))])
      (values v idx)))
  (λ (a b)
    (< (hash-ref hsh a) (hash-ref hsh b))))
