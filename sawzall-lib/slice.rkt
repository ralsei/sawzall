#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract/base
         racket/match
         racket/set
         syntax/parse/define
         "grouped-df.rkt"
         "grouping.rkt"
         "slice-spec.rkt")
(provide slice
         everything starting-with ending-with containing
         and or not
         all-in any-in

         take-rows)

(define (slice-df df parsed-spec groups)
  (define to-copy (exec-spec-on-df df parsed-spec))
  (when (not (set-empty? (set-intersect to-copy groups)))
    (error 'slice "cannot remove grouping variable from grouped data-frame"))

  (define return-df (make-data-frame))

  (for ([s (in-list to-copy)])
    (df-add-series! return-df (df-duplicate-series df s)))
  return-df)

(define-syntax-parse-rule (slice df spec:slice-spec)
  #:declare df (expr/c #'(or/c data-frame? grouped-data-frame?))
  (ignore-groups-apply (λ (x grps) (slice-df x spec.parsed grps)) df.c #:pass-groups? #t))

(define (take-rows df beg end)
  (grouped-df-apply (λ (x) (take-rows-df x beg end)) df))

(define (take-rows-df df beg end)
  (match-define (sub-data-frame int-df (ivl b e)) df)
  (df-dumb-copy/sub (df-with-ivl int-df (ivl (+ beg b) (+ end b)))))
