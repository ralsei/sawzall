#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract
         racket/function
         racket/match
         racket/vector
         "grouped-df.rkt"
         "grouping.rkt"
         "syntax.rkt")
(provide create)

(define-syntax (create stx)
  (column-syntax-form stx #'create/int #t))

(define/contract (create/int df proc)
  (-> (or/c data-frame? grouped-data-frame?) column-proc? (or/c data-frame? grouped-data-frame?))
  (grouped-df-apply (create-on-df _ proc) df))

(define (create-on-df df proc)
  (match-define (column-proc new-cols binders procs) proc)
  (match-define (sub-data-frame internal-df (ivl beg end)) df)

  (define return-df (df-shallow-copy internal-df))

  ; we have to support sequential saw-λ
  (for ([col-name (in-list new-cols)]
        [binder (in-list binders)]
        [to-apply (in-list procs)])
    ; we need to map if there is a single element being bound
    (define all-vector? (andmap (λ (x) (eq? (cdr x) 'vector)) binder))

    (define func
      (if all-vector?
          to-apply
          (curry vector-map to-apply)))

    (define len (df-row-count (sub-data-frame-delegate-frame df)))
    (define args
      (if all-vector?
          (map (compose (df-select/sub return-df _) car) binder)
          (for/list ([binding (in-list binder)])
            (define var (car binding))
            (define ty (cdr binding))
            (if (eq? ty 'vector)
                (make-vector len (df-select/sub return-df var))
                (df-select/sub return-df var)))))

    (df-add-series!
     return-df
     (make-series col-name #:data (apply func args))))

  return-df)
