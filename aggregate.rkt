#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/match
         racket/vector
         "grouping.rkt"
         "saw-lambda.rkt")
(provide
 (contract-out [aggregate (-> (or/c data-frame? grouped-data-frame?)
                              saw-proc?
                              (or/c data-frame? grouped-data-frame?))]))

; summarizes a given data-frame into the given result by the saw-lambda, after splitting by group
(define (aggregate df proc)
  (ungroup (group-map (aggregate-already-split _ _ proc) df #:pass-groups? #t)))

; after already having split the data-frame up, aggregate the results
(define (aggregate-already-split df retain proc)
  (match-define (saw-proc new-cols binders procs) proc)

  (define return-df (make-data-frame))
  (define retain-series
    (for/list ([v (in-list retain)])
      ; should be homogenous
      (make-series v #:data (vector-take (df-select df v) 1))))
  (define new-series
    (for/list ([new-col (in-list new-cols)]
               [binder (in-list binders)]
               [to-apply (in-list procs)])
      (make-series new-col #:data (vector
                                   (apply to-apply (map (df-select df _) binder))))))

  (for ([s (in-list (append retain-series new-series))])
    (df-add-series! return-df s))
  return-df)
