#lang racket/base
(require data-frame
         fancy-app
         racket/contract
         "helpers.rkt"
         "grouping.rkt"
         "reorder-df.rkt")
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

(define (by-vector vec)
  (define hsh
    (for/hash ([(v idx) (in-indexed (in-vector vec))])
      (values v idx)))
  (λ (a b)
    (< (hash-ref hsh a) (hash-ref hsh b))))
