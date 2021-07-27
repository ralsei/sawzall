#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract/base
         racket/set
         syntax/parse/define
         "grouped-df.rkt"
         "grouping.rkt"
         "slice-spec.rkt")
(provide slice
         everything starting-with ending-with containing
         and or not
         all-in any-in)

(define (slice-df df parsed-spec groups)
  (define to-copy (exec-spec-on-df df parsed-spec))
  (when (not (set-empty? (set-intersect to-copy (apply set groups))))
    (error 'slice "cannot remove grouping variable from grouped data-frame"))

  (define return-df (make-data-frame))

  (for ([s (in-set to-copy)])
    (df-add-series! return-df (df-duplicate-series df s)))
  return-df)

(define-syntax-parse-rule (slice df spec:slice-spec)
  #:declare df (expr/c #'(or/c data-frame? grouped-data-frame?))
  (ignore-groups-apply (λ (x grps) (slice-df x spec.parsed grps)) df.c #:pass-groups? #t))
