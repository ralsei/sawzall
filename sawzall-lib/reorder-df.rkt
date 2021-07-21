#lang racket/base
(require data-frame
         fancy-app
         racket/vector
         "helpers.rkt")
(provide reorder-df
         reorder-default)

;; sorts a data-frame with the given columns and comparators
;; breaks ties using subsequent columns
;;
;; XXX: this function is used internally a lot. maybe it shouldn't be!
;;      Julia uses an index vector for grouping as metadata
(define (reorder-df df pairs)
  (define row-count (df-row-count df))

  (define index-vector (build-vector row-count (λ (x) x)))
  (for ([p (in-list (reverse pairs))])
    (define col (car p))
    (define cmp? (cdr p))

    (define data (df-select df col))
    (vector-sort! index-vector cmp? #:key (vector-ref data _)))

  ;; this is non-deterministic, but since we define it here, we can use it as a commonality
  ;; to zip the vectors later
  ;;
  ;; MOST TIME here is spent in `vector-reorder!`
  (define series (df-series-names df))
  (define reordered-vecs
    (for/vector ([col (in-list series)])
      (define data (df-select df col))
      (vector-reorder! data index-vector)
      data))

  (define return-df (make-data-frame))
  (for ([name (in-list series)]
        [data (in-vector reordered-vecs)])
    (df-add-series! return-df (make-series name #:data data)))

  return-df)

;; like the above, but defaults to orderable<? for all columns
(define (reorder-default df cols)
  (reorder-df df (map (cons _ orderable<?) cols)))
