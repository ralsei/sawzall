#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/match
         "grouping.rkt"
         "saw-lambda.rkt")
(provide
 (contract-out [aggregate (-> (or/c data-frame? grouped-data-frame?)
                              saw-proc?
                              (or/c data-frame? grouped-data-frame?))]))

; summarizes a given data-frame into the given result by the saw-lambda, after splitting by group
(define (aggregate df proc)
  (define res
    (for/list ([v (in-list (get-frames df))])
      (aggregate-already-split v (get-groups df) proc)))
  (return-with-list df res #:peel? #t))

; after already having split the data-frame up, aggregate the results
; assumes that the columns in `retain` only have one value
(define (aggregate-already-split df retain proc)
  (match-define (saw-proc new-cols binders procs) proc)

  (define return-df (make-data-frame))
  (define retain-series
    (for/list ([group (in-list retain)])
      ; we know that facet from the above call should make only one possibility present,
      ; so just get one element
      (make-series group #:data (df-select df group #:stop 1))))

  (define new-series
    (for/list ([new-col (in-list new-cols)]
               [binder (in-list binders)]
               [to-apply (in-list procs)])
      (make-series new-col #:data (vector
                                   (apply to-apply (map (df-select df _) binder))))))

  (for ([s (in-list (append retain-series new-series))])
    (df-add-series! return-df s))
  return-df)
