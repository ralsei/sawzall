#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract/base
         racket/function
         racket/match
         racket/set
         racket/sequence
         racket/string
         syntax/parse/define)
(provide (for-syntax slice-spec)
         exec-spec exec-spec-on-df
         everything starting-with ending-with containing
         and or not
         all-in any-in

         everything$ starting-with$ ending-with$ containing$
         and$ or$ not$
         all-in$ any-in$)

;; a Slice-Spec is one of:
;; - String
;; - Regex
;; - everything                   ; all variables
;; - [String ...]                 ; multiple variables
;; - (all-in [Sequenceof String]) ; all from string sequence
;; - (any-in [Sequenceof String]) ; any from string sequence
;; - (or Slice-Spec ...)          ; union
;; - (and Slice-Spec ...)         ; intersection
;; - (not Slice-Spec)             ; complement
;; - (starting-with String)
;; - (ending-with String)
;; - (containing String)
(struct everything$ () #:transparent)
(struct multi-var$ (names) #:transparent)
(struct any-in$ (sequence) #:transparent)
(struct all-in$ (sequence) #:transparent)
(struct or$ (slice-specs) #:transparent)
(struct and$ (slice-specs) #:transparent)
(struct not$ (slice-spec) #:transparent)
(struct starting-with$ (prefix) #:transparent)
(struct ending-with$ (suffix) #:transparent)
(struct containing$ (substring) #:transparent)

(define-syntax-parse-rule (define-dummy-stxes name:id ...+)
  (begin
    (define-syntax (name stx)
      (raise-syntax-error #f "cannot be used outside of a slice specification" stx))
    ...))

(define-dummy-stxes everything starting-with ending-with containing all-in any-in)

(begin-for-syntax
  (define-syntax-class slice-spec
    #:attributes (parsed)
    #:literals (everything
                starting-with ending-with containing
                or and not
                all-in any-in)
    [pattern everything
             #:with parsed #'(everything$)]
    [pattern (starting-with prefix)
             #:declare prefix (expr/c #'string?)
             #:with parsed #'(starting-with$ prefix.c)]
    [pattern (ending-with suffix)
             #:declare suffix (expr/c #'string?)
             #:with parsed #'(ending-with$ suffix.c)]
    [pattern (containing substr)
             #:declare substr (expr/c #'string?)
             #:with parsed #'(containing$ substr.c)]
    [pattern (or spec:slice-spec ...+)
             #:with parsed #'(or$ (list spec.parsed ...))]
    [pattern (and spec:slice-spec ...+)
             #:with parsed #'(and$ (list spec.parsed ...))]
    [pattern (not spec:slice-spec)
             #:with parsed #'(not$ spec.parsed)]
    [pattern (all-in sequence)
             #:declare sequence (expr/c #'(sequence/c string?))
             #:with parsed #'(all-in$ sequence.c)]
    [pattern (any-in sequence)
             #:declare sequence (expr/c #'(sequence/c string?))
             #:with parsed #'(any-in$ sequence.c)]
    [pattern [var:string ...+]
             #:with parsed #'(multi-var$ (list var ...))]
    [pattern var
             #:declare var (expr/c #'(or/c string? regexp?))
             #:with parsed #'var.c]))

(define (exec-spec universe parsed-spec)
  (define (in-universe? var)
    (set-member? universe var))
  (match parsed-spec
    [(? string? var)
     (when (not (in-universe? var))
       (error 'exec-spec "selection not in universe: ~a" var))
     (list var)]
    [(? regexp? rx) (filter (curry regexp-match? rx) universe)]
    [(everything$) universe]
    [(starting-with$ pref) (filter (string-prefix? _ pref) universe)]
    [(ending-with$ suff) (filter (string-suffix? _ suff) universe)]
    [(containing$ substr) (filter (string-contains? _ substr) universe)]
    [(or$ specs) (apply set-union (map (curry exec-spec universe) specs))]
    [(and$ specs) (apply set-intersect (map (curry exec-spec universe) specs))]
    [(not$ spec) (set-subtract universe (exec-spec universe spec))]
    [(all-in$ sequence)
     (define lst (sequence->list sequence))
     (when (not (andmap in-universe? lst))
       (error 'exec-spec "some selection(s) not in universe: ~a" lst))
     lst]
    [(any-in$ sequence)
     (sequence->list (sequence-filter in-universe? sequence))]
    [(multi-var$ vars)
     (when (not (andmap in-universe? vars))
       (error 'exec-spec "some selection(s) not in universe: ~a" vars))
     (apply list vars)]
    [_ (error 'exec-spec "invalid slice specification: ~a" parsed-spec)]))

(define (exec-spec-on-df df parsed-spec)
  (exec-spec (df-series-names df) parsed-spec))
