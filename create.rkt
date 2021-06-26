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
  (match-define (saw-proc new-cols binders procs) proc)
  (return-with-list
   df
   (for/list ([v (in-list (get-frames df))])
     (define return-df (df-shallow-copy v)) ; UNDOCUMENTED

     ; we have to support sequential saw-λ
     (for ([col-name (in-list new-cols)]
           [binder (in-list binders)]
           [to-apply (in-list procs)])
        (df-add-series!
         return-df
         (make-series col-name #:data (apply (curry vector-map to-apply)
                                             (map (df-select return-df _) binder)))))
     return-df)))

(define (create-all df proc)
  (match-define (saw-proc new-cols binders procs) proc)
  (return-with-list
   df
   (for/list ([v (in-list (get-frames df))])
    (define return-df (df-shallow-copy v))

    (for ([col-name (in-list new-cols)]
          [binder (in-list binders)]
          [to-apply (in-list procs)])
      (df-add-series!
       return-df
       (make-series col-name #:data (apply to-apply (map (df-select return-df _) binder)))))
    return-df)))
