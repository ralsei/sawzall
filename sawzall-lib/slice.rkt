#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract
         racket/function
         racket/match
         racket/set
         racket/string
         syntax/parse/define
         "grouped-df.rkt"
         "grouping.rkt")
(provide slice
         everything columns starting-with ending-with containing matching
         and or not)

(define (set-filter f? s) (for/set ([v (in-set s)] #:when (f? v)) v))

;; a Slice-Spec is one of:
;; - String
;; - everything           ; all variables
;; - (columns String ...) ; multiple variables
;; - (or Slice-Spec ...)  ; union
;; - (and Slice-Spec ...) ; intersection
;; - (not Slice-Spec)     ; complement
;; - (starting-with String)
;; - (ending-with String)
;; - (containing String)
;; - (matching Regex)

(define (exec-spec universe quoted-spec)
  (match quoted-spec
    [(? string? var) (set var)]
    ['everything universe]
    [`(starting-with ,pref) (set-filter (string-prefix? _ pref) universe)]
    [`(ending-with ,suff) (set-filter (string-suffix? _ suff) universe)]
    [`(containing ,substr) (set-filter (string-contains? _ substr) universe)]
    [`(matching ,rx) (set-filter (curry regexp-match? rx) universe)]
    [`(columns . ,vars) (apply set vars)]
    [`(or . ,specs) (apply set-union (map (curry exec-spec universe) specs))]
    [`(and . ,specs) (apply set-intersect (map (curry exec-spec universe) specs))]
    [`(not ,spec) (set-subtract universe (exec-spec universe spec))]
    [_ (error 'exec-spec "invalid slice specification: ~a" quoted-spec)]))

(define (slice-df df quoted-spec groups)
  (define to-copy (exec-spec (apply set (df-series-names df)) quoted-spec))
  (when (not (set-empty? (set-intersect to-copy (apply set groups))))
    (error 'slice "cannot remove grouping variable from grouped data-frame"))

  (define return-df (make-data-frame))

  (for ([s (in-set to-copy)])
    (df-add-series! return-df (df-duplicate-series df s)))
  return-df)

(define-syntax-parse-rule (define-dummy-stxes name:id ...+)
  (begin
    (define-syntax (name stx)
      (raise-syntax-error #f "cannot be used outside of a slice specification" stx))
    ...))

(define-dummy-stxes everything starting-with ending-with containing matching columns)

(begin-for-syntax
  (define-syntax-class slice-spec
    #:literals (everything
                starting-with ending-with containing matching
                columns or and not)
    [pattern everything]
    [pattern (starting-with str)
             #:declare str (expr/c #'string?)]
    [pattern (ending-with str)
             #:declare str (expr/c #'string?)]
    [pattern (containing str)
             #:declare str (expr/c #'string?)]
    [pattern (matching rx)
             #:declare rx (expr/c #'regexp?)]
    [pattern (columns str ...+)
             #:declare str (expr/c #'string?)]
    [pattern (or spec:slice-spec ...+)]
    [pattern (and spec:slice-spec ...+)]
    [pattern (not spec:slice-spec)]
    [pattern var #:declare var (expr/c #'string?)]))

(define-syntax-parse-rule (slice df spec:slice-spec)
  #:declare df (expr/c #'(or/c data-frame? grouped-data-frame?))
  (ignore-groups-apply (λ (x grps) (slice-df x 'spec grps)) df #:pass-groups? #t))