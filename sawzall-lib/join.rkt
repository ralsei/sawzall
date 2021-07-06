#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/vector
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
                                        (or/c data-frame? grouped-data-frame?))]))

(define (left-join df1 df2 by #:cmp? [cmp? orderable<?])
  (ignore-grouping (left-join-dfs _ (ungroup-all df2) by cmp?) df1))
(define (right-join df1 df2 by #:cmp? [cmp? orderable<?])
  (ignore-grouping (left-join-dfs (ungroup-all df2) _ by cmp?) df1))

(define (split-with/cons df group)
  (define (df-with possibility)
    (define return-df (make-data-frame))
    ; TODO: data-frame uses bsearch for this
    (define possibility-indices
      (for/list ([(v idx) (in-indexed (in-vector (df-select df group)))]
                 #:when (equal? v possibility))
        idx))
    (define new-series
      (for/list ([col (in-list (df-series-names df))])
        (make-series col #:data (for/vector ([idx (in-list possibility-indices)])
                                  (df-ref df idx col)))))
    (for ([s (in-list new-series)])
      (df-add-series! return-df s))
    (cons possibility return-df))
  (vector-map df-with (possibilities df group)))

; takes all rows from df1, and all columns from df1 and df2. rows in df1 with no match in df2
; will have #f values in the new columns.
;
; XXX: is there a way to determine what the actual NA value is, or do we just have to guess?
(define (left-join-dfs df1 df2 by cmp?)
  (define df1-sorted (reorder/int df1 (sort-proc (list by) (list cmp?))))
  (define df2-sorted (reorder/int df2 (sort-proc (list by) (list cmp?))))

  (define df1-split (split-with/cons df1-sorted by))
  (define df2-split (split-with/cons df2-sorted by))

  (let loop ([df1-idx 0] [df2-idx 0] [dfs '()])
    (cond [(>= df1-idx (vector-length df1-split)) (apply combine (reverse dfs))]
          [(>= df2-idx (vector-length df2-split))
           (loop (add1 df1-idx) df2-idx
                 (cons (join-no-matches (cdr (vector-ref df1-split df1-idx))
                                        (df-series-names df2))
                       dfs))]
          [(equal? (car (vector-ref df1-split df1-idx))
                   (car (vector-ref df2-split df2-idx)))
           (loop (add1 df1-idx) (add1 df2-idx)
                 (cons (join-matches (cdr (vector-ref df1-split df1-idx))
                                     (cdr (vector-ref df2-split df2-idx))
                                     by)
                       dfs))]
          [(cmp? (car (vector-ref df1-split df1-idx))
                 (car (vector-ref df2-split df2-idx)))
           (loop (add1 df1-idx) df2-idx
                 (cons (join-no-matches (cdr (vector-ref df1-split df1-idx))
                                        (df-series-names df2))
                       dfs))]
          [else (loop df1-idx (add1 df2-idx) dfs)])))

; just pad missing data with #f
(define (join-no-matches df1 df2-series)
  (define return-df (make-data-frame))
  (define df1-size (df-row-count df1))

  (for ([name (in-list df2-series)])
    (df-add-series! return-df (make-series name #:data (make-vector df1-size #f))))
  (for ([name (in-list (df-series-names df1))])
    (df-add-series! return-df (make-series name #:data (df-select df1 name))))

  return-df)

; cobine on all shared matches
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
