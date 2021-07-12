#lang racket/base
(require data-frame
         racket/contract/base
         racket/set
         racket/vector
         threading)
(provide possibilities vector-reorder orderable? df-na-value
         (contract-out [orderable<? (-> orderable? orderable? boolean?)]))

; determines the NA value in a given df series
(define (df-na-value df col)
  (series-na (df-duplicate-series df col)))

; removes duplicates from a given vector
(define (vector-remove-duplicates vec)
  (define seen (mutable-set))
  (for/vector ([v (in-vector vec)]
               #:unless (set-member? seen v))
    (set-add! seen v)
    v))

; determines the possible values that a given data-frame has in a column
(define (possibilities data group)
  (vector-remove-duplicates (df-select data group)))

; reorders a vector based on the given indices
; example:
;   (vector-reorder (vector 1 2 3) (vector 2 1 0))
;   => (vector 3 2 1)
(define (vector-reorder vec indices)
  (when (not (= (vector-length indices) (vector-length vec)))
    (error 'vector-reorder "index list not same length as vector"))
  (for/vector ([idx (in-vector indices)])
    (vector-ref vec idx)))

; inferred generic comparator
(define (orderable-major v)
  (cond [(boolean? v)    0]
        [(char? v)       1]
        [(real? v)       2]
        [(symbol? v)     3]
        [(keyword? v)    4]
        [(string? v)     5]
        [(null? v)       6]
        [(void? v)       7]
        [(eof-object? v) 8]
        [else #f]))

(define (orderable? v) (and (orderable-major v) #t))

(define (orderable<? a b)
  (let ([am (orderable-major a)]
        [bm (orderable-major b)])
    (cond [(or (not am) (not bm)) #f]
          [(= am bm)
           (cond [(boolean? a) (not a)]
                 [(char? a) (char<? a b)]
                 [(real? a) (< a b)]
                 [(symbol? a)
                  (cond [(symbol-interned? a)
                         (and (symbol-interned? b)
                              (symbol<? a b))]
                        [(symbol-interned? b) #t]
                        [(symbol-unreadable? a)
                         (and (symbol-unreadable? b)
                              (symbol<? a b))]
                        [(symbol-unreadable? b) #t]
                        [else (symbol<? a b)])]
                 [(keyword? a) (keyword<? a b)]
                 [(string? a) (string<? a b)]
                 [else (error 'orderable<? "cannot order: please specify a comparator")])]
          [else (< am bm)])))
