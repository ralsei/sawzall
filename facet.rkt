#lang racket/base
(require data-frame
         fancy-app
         racket/set
         racket/vector
         threading)
(provide facet)

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

; defines the `facet` operation, which constructs multiple data-frames from
; an existing data-frame.
; does NOT produce another data-frame, instead producing a list of data-frames
; split into unique groups.
(define (facet df group)
  (define (df-with possibility)
    (define return-df (make-data-frame))
    (define possibility-indices
      (for/list ([v (in-vector (df-select df group #:filter (equal? _ possibility)))])
        (df-index-of df group v)))
    (define new-series
      (for/list ([col (in-list (df-series-names df))])
        (make-series col #:data (for/vector ([idx (in-list possibility-indices)])
                                  (df-ref df idx col)))))
    (for ([s (in-list new-series)])
      (df-add-series! return-df s))
    return-df)
  (vector-map df-with (possibilities df group)))
