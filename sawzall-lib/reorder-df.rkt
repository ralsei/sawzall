#lang racket/base
(require data-frame
         fancy-app
         racket/vector
         "helpers.rkt")
(provide reorder-df)

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
    (df-set-sorted! return-df col cmp?))
  return-df)
