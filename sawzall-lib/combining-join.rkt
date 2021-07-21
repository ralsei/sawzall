#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/list
         "helpers.rkt"
         "grouped-df.rkt"
         "grouping.rkt"
         "split.rkt")
(provide (contract-out [left-join (-> (or/c data-frame? grouped-data-frame?)
                                      (or/c data-frame? grouped-data-frame?)
                                      string? ...
                                      (or/c data-frame? grouped-data-frame?))]
                       [right-join (-> (or/c data-frame? grouped-data-frame?)
                                       (or/c data-frame? grouped-data-frame?)
                                       string? ...
                                       (or/c data-frame? grouped-data-frame?))]
                       [inner-join (-> (or/c data-frame? grouped-data-frame?)
                                       (or/c data-frame? grouped-data-frame?)
                                       string? ...
                                       (or/c data-frame? grouped-data-frame?))]
                       [full-join (-> (or/c data-frame? grouped-data-frame?)
                                      (or/c data-frame? grouped-data-frame?)
                                      string? ...
                                      (or/c data-frame? grouped-data-frame?))]))

(define (left-join df1 df2 . by)
  (ignore-groups-apply (left-join-dfs _ (ungroup df2) by) df1))
(define (right-join df1 df2 . by)
  (ignore-groups-apply (left-join-dfs (ungroup df2) _ by) df1))
(define (inner-join df1 df2 . by)
  (ignore-groups-apply (inner-join-dfs _ (ungroup df2) by) df1))
(define (full-join df1 df2 . by)
  (ignore-groups-apply (full-join-dfs _ (ungroup df2) by) df1))

; pad any missing data that isn't matched in any column in df2 with #f
(define (join-no-matches df1 df2-series)
  (define df1-int (sub-data-frame-delegate-frame df1))
  (define return-df (make-data-frame))
  (define df1-size (df-row-count/sub df1))

  (for ([name (in-list df2-series)])
    (df-add-series! return-df (make-series name #:data (make-vector df1-size #f))))
  (for ([name (in-list (df-series-names df1-int))])
    (df-add-series! return-df (make-series name #:data (df-select/sub df1 name))))

  return-df)

; combine on all shared matches
(define (join-matches df1 df2 by)
  (define df1-int (sub-data-frame-delegate-frame df1))
  (define df2-int (sub-data-frame-delegate-frame df2))

  (define (permute-data series df2?)
    (for*/vector ([df1-val (in-data-frame/sub df1 (if df2? (first by) series))]
                  [df2-val (in-data-frame/sub df2 (if df2? series (first by)))])
      (if df2? df2-val df1-val)))
  
  (define return-df (make-data-frame))
  (for ([name (in-list (df-series-names df2-int))])
    (df-add-series! return-df (make-series name #:data (permute-data name #t))))
  (for ([name (in-list (df-series-names df1-int))])
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
(define ((combining-join on-df2-end on-lt on-else) df1 df2 by-int)
  (define by
    (if (null? by-int)
        (shared-series (list df1 df2))
        by-int))

  (define df1-grouped (apply group-with df1 by))
  (define df2-grouped (apply group-with df2 by))
  (define df1-sorted (grouped-data-frame-delegate-frame df1-grouped))
  (define df2-sorted (grouped-data-frame-delegate-frame df2-grouped))
  (define df1-group-ivls (first (grouped-data-frame-group-indices df1-grouped)))
  (define df2-group-ivls (first (grouped-data-frame-group-indices df2-grouped)))

  (define df1-by (get-grouped-by df1-grouped))
  (define df2-by (get-grouped-by df2-grouped))

  (define df1-len (vector-length df1-by))
  (define df2-len (vector-length df2-by))

  ; the return of merge from the hit series merge sort
  (let loop ([df1-idx 0] [df2-idx 0] [dfs '()])
    (cond [(>= df1-idx df1-len)
           ; we've run out of vector to use, so return the final df
           (apply combine (reverse dfs))]
          [(>= df2-idx df2-len)
           ; we've run out of the second vector. this varies between joins
           (loop (add1 df1-idx) df2-idx
                 (on-df2-end (df-with-ivl df1-sorted (vector-ref df1-group-ivls df1-idx))
                             (df-series-names df2)
                             dfs))]
          [(equal? (vector-ref df1-by df1-idx)
                   (vector-ref df2-by df2-idx))
           ; the rows share the same key, so merge them with combining
           (loop (add1 df1-idx) (add1 df2-idx)
                 (cons (join-matches (df-with-ivl df1-sorted (vector-ref df1-group-ivls df1-idx))
                                     (df-with-ivl df2-sorted (vector-ref df2-group-ivls df2-idx))
                                     by)
                       dfs))]
          [(lexicographic-vector<? (vector-ref df1-by df1-idx)
                                   (vector-ref df2-by df2-idx))
           ; df1 < df2, so keep incrementing the df1 index until they match, and
           ; update the accumulator (varies)
           (loop (add1 df1-idx) df2-idx
                 (on-lt (df-with-ivl df1-sorted (vector-ref df1-group-ivls df1-idx))
                        (df-series-names df2)
                        dfs))]
          [else
           ; df1 > df2, so keep incrementing the df2 index until they match, and
           ; update the accumulator (again, varies)
           (loop df1-idx (add1 df2-idx)
                 (on-else (df-series-names df1)
                          (df-with-ivl df2-sorted (vector-ref df2-group-ivls df2-idx))
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

; gets what the given grouped data frame is grouped by at the top level, based on what
; we already have grouped
;
; by the implementation of a grouped data frame this set is already lexicographically sorted
(define (get-grouped-by gdf)
  (for/vector ([iv (in-vector (first (grouped-data-frame-group-indices gdf)))])
    (apply df-ref*
           (grouped-data-frame-delegate-frame gdf)
           (ivl-beg iv)
           (reverse (grouped-data-frame-groups gdf)))))
