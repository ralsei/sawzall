#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         "helpers.rkt"
         "grouping.rkt"
         "reorder.rkt"
         "split.rkt"
         "syntax.rkt")
(provide (contract-out [left-join (->* ((or/c data-frame? grouped-data-frame?)
                                        (or/c data-frame? grouped-data-frame?)
                                        string?)
                                       (#:cmp? (-> any/c any/c boolean?))
                                       (or/c data-frame? grouped-data-frame?))]
                       [right-join (->* ((or/c data-frame? grouped-data-frame?)
                                         (or/c data-frame? grouped-data-frame?)
                                         string?)
                                        (#:cmp? (-> any/c any/c boolean?))
                                        (or/c data-frame? grouped-data-frame?))]
                       [inner-join (->* ((or/c data-frame? grouped-data-frame?)
                                         (or/c data-frame? grouped-data-frame?)
                                         string?)
                                        (#:cmp? (-> any/c any/c boolean?))
                                        (or/c data-frame? grouped-data-frame?))]
                       [full-join (->* ((or/c data-frame? grouped-data-frame?)
                                        (or/c data-frame? grouped-data-frame?)
                                        string?)
                                       (#:cmp? (-> any/c any/c boolean?))
                                       (or/c data-frame? grouped-data-frame?))]))

(define (left-join df1 df2 by #:cmp? [cmp? orderable<?])
  (ignore-grouping (left-join-dfs _ (ungroup df2) by cmp?) df1))
(define (right-join df1 df2 by #:cmp? [cmp? orderable<?])
  (ignore-grouping (left-join-dfs (ungroup df2) _ by cmp?) df1))
(define (inner-join df1 df2 by #:cmp? [cmp? orderable<?])
  (ignore-grouping (inner-join-dfs _ (ungroup df2) by cmp?) df1))
(define (full-join df1 df2 by #:cmp? [cmp? orderable<?])
  (ignore-grouping (full-join-dfs _ (ungroup df2) by cmp?) df1))

; pad any missing data that isn't matched in any column in df2 with #f
(define (join-no-matches df1 df2-series)
  (define return-df (make-data-frame))
  (define df1-size (df-row-count df1))

  (for ([name (in-list df2-series)])
    (df-add-series! return-df (make-series name #:data (make-vector df1-size #f))))
  (for ([name (in-list (df-series-names df1))])
    (df-add-series! return-df (make-series name #:data (df-select df1 name))))

  return-df)

; combine on all shared matches
(define (join-matches df1 df2 by)
  (define (permute-data series df2?)
    (for*/vector ([df1-val (in-data-frame df1 (if df2? by series))]
                  [df2-val (in-data-frame df2 (if df2? series by))])
      (if df2? df2-val df1-val)))
  
  (define return-df (make-data-frame))
  (for ([name (in-list (df-series-names df2))])
    (df-add-series! return-df (make-series name #:data (permute-data name #t))))
  (for ([name (in-list (df-series-names df1))])
    (df-add-series! return-df (make-series name #:data (permute-data name #f))))

  return-df)

; defines a generic combining join
;
; (on-df2-end df1 df2-names acc): determines what to do with `df1`, the rows of the first df,
; `df2-names`, the series names of the second df, and `acc`, the accumulator when df2 ends
; (on-lt df1 df2-names acc): determines what to do with `df1`, the rows of the first df,
; `df2-names`, the series names of the second df, and `acc`, the accumulator when df1 < df2
; (on-else df1-names df2 acc): determines what to do with `df2-names`, the series names of the
; first df, `df2`, the rows of the second df, and `acc`, the accumulator when df1 > df2
;
; when df1 ends, we terminate and combine all results
; when the values are equal, we always call `join-matches`
;
; XXX: optimizations that could be done:
; - combining data-frames is costly and requires a lot of `vector-append`. we could probably use
; a "window"/"lens" into the data-frame and then iterate over series
; - split-with should use binary search if the series is sorted
(define ((combining-join on-df2-end on-lt on-else) df1 df2 by cmp?)
  ; sort and split, to make merge work. if we don't split, we can't handle duplicate keys
  (define df1-sorted (reorder df1 (cons by cmp?)))
  (define df2-sorted (reorder df2 (cons by cmp?)))

  (define df1-split (split-with-possibility df1-sorted by))
  (define df2-split (split-with-possibility df2-sorted by))

  ; the return of merge from the hit series merge sort
  (let loop ([df1-idx 0] [df2-idx 0] [dfs '()])
    (cond [(>= df1-idx (vector-length df1-split))
           ; we've run out of vector to use, so return the final df
           (apply combine (reverse dfs))]
          [(>= df2-idx (vector-length df2-split))
           ; we've run out of the second vector. this varies between joins
           (loop (add1 df1-idx) df2-idx
                 (on-df2-end (cdr (vector-ref df1-split df1-idx))
                             (df-series-names df2)
                             dfs))]
          [(equal? (car (vector-ref df1-split df1-idx))
                   (car (vector-ref df2-split df2-idx)))
           ; the rows share the same key, so merge them with combining
           (loop (add1 df1-idx) (add1 df2-idx)
                 (cons (join-matches (cdr (vector-ref df1-split df1-idx))
                                     (cdr (vector-ref df2-split df2-idx))
                                     by)
                       dfs))]
          [(cmp? (car (vector-ref df1-split df1-idx))
                 (car (vector-ref df2-split df2-idx)))
           ; df1 < df2, so keep incrementing the df1 index until they match, and
           ; update the accumulator (varies)
           (loop (add1 df1-idx) df2-idx
                 (on-lt (cdr (vector-ref df1-split df1-idx))
                        (df-series-names df2)
                        dfs))]
          [else
           ; df1 > df2, so keep incrementing the df2 index until they match, and
           ; update the accumulator (again, varies)
           (loop df1-idx (add1 df2-idx)
                 (on-else (df-series-names df1)
                          (cdr (vector-ref df2-split df2-idx))
                          dfs))])))

(define left-join-dfs
  (combining-join
   ; if df2 ends, keep adding #f
   (λ (df1 df2-names acc)
     (cons (join-no-matches df1 df2-names) acc))
   ; if df1 < df2, add #f
   (λ (df1 df2-names acc)
     (cons (join-no-matches df1 df2-names) acc))
   ; if df1 > df2, do nothing
   (λ (df1-names df2 acc) acc)))

(define inner-join-dfs
  ; only do something if we're equal
  (combining-join
   (λ (df1 df2-names acc) acc)
   (λ (df1 df2-names acc) acc)
   (λ (df1-names df2 acc) acc)))

(define full-join-dfs
  ; keep adding #f no matter what
  (combining-join
   (λ (df1 df2-names acc)
     (cons (join-no-matches df1 df2-names) acc))
   (λ (df1 df2-names acc)
     (cons (join-no-matches df1 df2-names) acc))
   (λ (df1-names df2 acc)
     (cons (join-no-matches df2 df1-names) acc))))
