#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/function
         racket/match
         racket/vector
         "saw-lambda.rkt")
(provide
 (contract-out [create (-> data-frame? saw-proc? data-frame?)]))

(define (create df proc)
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
