#lang racket/base
(require data-frame
         racket/set
         racket/vector
         threading)
(provide possibilities)

; removes duplicates from a given vector
(define (vector-remove-duplicates vec)
  (define seen (mutable-set))
  (for/vector ([v (in-vector vec)]
               #:unless (set-member? seen v))
    (set-add! seen v)
    v))

; determines the possible values that a given data-frame has in a column
(define (possibilities data group)
  (~> (df-select data group)
      vector-remove-duplicates
      (vector-filter (λ (x) (and x #t)) _)))
