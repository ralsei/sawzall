#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract
         racket/function
         racket/match
         racket/vector
         "grouping.rkt"
         "syntax.rkt")
(provide create create-all)

(define-syntax (create stx)
  (column-syntax-form stx #'create/int))

(define/contract (create/int df proc)
  (-> (or/c data-frame? grouped-data-frame?) column-proc? (or/c data-frame? grouped-data-frame?))
  (group-map (create-on-df _ proc) df))

(define (create-on-df df proc)
  (match-define (column-proc new-cols binders procs) proc)
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

(define-syntax (create-all stx)
  (column-syntax-form stx #'create-all/int))

(define (create-all/int df proc)
  (group-map (create-all-on-df _ proc) df))

(define (create-all-on-df df proc)
  (match-define (column-proc new-cols binders procs) proc)
  (define return-df (df-shallow-copy df))

  (for ([col-name (in-list new-cols)]
        [binder (in-list binders)]
        [to-apply (in-list procs)])
    (df-add-series!
     return-df
     (make-series col-name #:data (apply to-apply (map (df-select return-df _) binder)))))

  return-df)
