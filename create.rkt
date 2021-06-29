#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/function
         racket/match
         racket/vector
         "grouping.rkt"
         "saw-lambda.rkt")
(provide
 (contract-out [create (-> (or/c data-frame? grouped-data-frame?)
                           saw-proc?
                           (or/c data-frame? grouped-data-frame?))]
               [create-all (-> (or/c data-frame? grouped-data-frame?)
                               saw-proc?
                               (or/c data-frame? grouped-data-frame?))]))

(define (create df proc)
  (group-map (create-internal _ proc) df))

(define (create-internal df proc)
  (match-define (saw-proc new-cols binders procs) proc)
  (define return-df (df-shallow-copy df)) ; UNDOCUMENTED

  ; we have to support sequential saw-λ
  (for ([col-name (in-list new-cols)]
        [binder (in-list binders)]
        [to-apply (in-list procs)])
    (df-add-series!
     return-df
     (make-series col-name #:data (apply (curry vector-map to-apply)
                                         (map (df-select return-df _) binder)))))
  return-df)

(define (create-all df proc)
  (group-map (create-all-internal _ proc) df))

(define (create-all-internal df proc)
  (match-define (saw-proc new-cols binders procs) proc)
  (define return-df (df-shallow-copy df))

  (for ([col-name (in-list new-cols)]
        [binder (in-list binders)]
        [to-apply (in-list procs)])
    (df-add-series!
     return-df
     (make-series col-name #:data (apply to-apply (map (df-select return-df _) binder)))))

  return-df)
