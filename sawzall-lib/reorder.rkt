#lang racket/base
(require data-frame
         fancy-app
         racket/contract
         "helpers.rkt"
         "grouped-df.rkt"
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

  (define (run-reorder-df maybe-sub-df)
    (define real-df
      (cond [(sub-data-frame? maybe-sub-df)
             ;; truncate and turn into a regular data-frame
             (df-dumb-copy/sub maybe-sub-df)]
            [else maybe-sub-df]))
    (reorder-df real-df pairs))

  ((if in-groups? grouped-df-apply ignore-groups-apply) run-reorder-df df))

(define (by-vector vec)
  (define hsh
    (for/hash ([(v idx) (in-indexed (in-vector vec))])
      (values v idx)))
  (λ (a b)
    (< (hash-ref hsh a) (hash-ref hsh b))))
