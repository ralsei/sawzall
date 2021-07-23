#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/function
         racket/match
         racket/set
         racket/string
         syntax/parse/define)
(provide (for-syntax slice-spec)
         exec-spec exec-spec-on-df
         everything starting-with ending-with containing
         and or not)

(define (set-filter f? s) (for/set ([v (in-set s)] #:when (f? v)) v))

;; a Slice-Spec is one of:
;; - String
;; - Regex
;; - everything           ; all variables
;; - [String ...]         ; multiple variables
;; - (or Slice-Spec ...)  ; union
;; - (and Slice-Spec ...) ; intersection
;; - (not Slice-Spec)     ; complement
;; - (starting-with String)
;; - (ending-with String)
;; - (containing String)

(define (exec-spec universe quoted-spec)
  (define (in-universe? var)
    (set-member? universe var))
  (match quoted-spec
    [(? string? var)
     (when (not (in-universe? var))
       (error 'exec-spec "selection not in universe: ~a" var))
     (set var)]
    [(? regexp? rx) (set-filter (curry regexp-match? rx) universe)]
    ['everything universe]
    [`(starting-with ,pref) (set-filter (string-prefix? _ pref) universe)]
    [`(ending-with ,suff) (set-filter (string-suffix? _ suff) universe)]
    [`(containing ,substr) (set-filter (string-contains? _ substr) universe)]
    [`(or . ,specs) (apply set-union (map (curry exec-spec universe) specs))]
    [`(and . ,specs) (apply set-intersect (map (curry exec-spec universe) specs))]
    [`(not ,spec) (set-subtract universe (exec-spec universe spec))]
    [`(,vars ...)
     (when (not (andmap in-universe? vars))
       (error 'exec-spec "some selection(s) not in universe: ~a" vars))
     (apply set vars)]
    [_ (error 'exec-spec "invalid slice specification: ~a" quoted-spec)]))

(define (exec-spec-on-df df quoted-spec)
  (exec-spec (apply set (df-series-names df)) quoted-spec))

(define-syntax-parse-rule (define-dummy-stxes name:id ...+)
  (begin
    (define-syntax (name stx)
      (raise-syntax-error #f "cannot be used outside of a slice specification" stx))
    ...))

(define-dummy-stxes everything starting-with ending-with containing)

(begin-for-syntax
  (define-syntax-class slice-spec
    #:literals (everything
                starting-with ending-with containing
                or and not)
    [pattern everything]
    [pattern (starting-with str)
             #:declare str (expr/c #'string?)]
    [pattern (ending-with str)
             #:declare str (expr/c #'string?)]
    [pattern (containing str)
             #:declare str (expr/c #'string?)]
    [pattern (or spec:slice-spec ...+)]
    [pattern (and spec:slice-spec ...+)]
    [pattern (not spec:slice-spec)]
    [pattern [var ...+] #:declare var (expr/c #'string?)]
    [pattern var #:declare var (expr/c #'(or/c string? regexp?))]))
