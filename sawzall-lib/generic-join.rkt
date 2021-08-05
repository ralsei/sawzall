#lang racket/base
(require data-frame
         fancy-app
         racket/list
         "helpers.rkt"
         "grouped-df.rkt"
         "grouping.rkt"
         "split.rkt")
(provide generic-join)

;; defines a generic join on two data-frames.
(define ((generic-join #:on-end on-end
                       #:on-= on-eq
                       #:on-< on-lt
                       #:on-> on-else)
         df1 df2 [by-int '()])
  (define by
    (if (null? by-int)
        (shared-series (list df1 df2))
        by-int))

  (when (null? by)
    (error 'join "no shared series between merge
please use rename to make some shared columns first"))

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
           (when (null? dfs)
             (error 'join "no data-frames to combine (are you using a semi-join with no matches?)"))
           (apply combine (reverse dfs))]
          [(>= df2-idx df2-len)
           ; we've run out of the second vector. this varies between joins
           (loop (add1 df1-idx) df2-idx
                 (on-end (df-with-ivl df1-sorted (vector-ref df1-group-ivls df1-idx))
                         (df-series-names df2)
                         dfs))]
          [(equal? (vector-ref df1-by df1-idx)
                   (vector-ref df2-by df2-idx))
           ; the rows share the same key, so merge them with combining
           (loop (add1 df1-idx) (add1 df2-idx)
                 (on-eq (df-with-ivl df1-sorted (vector-ref df1-group-ivls df1-idx))
                        (df-with-ivl df2-sorted (vector-ref df2-group-ivls df2-idx))
                        by
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

;; gets what the given grouped data frame is grouped by at the top level, based on what
;; we already have grouped
;;
;; by the implementation of a grouped data frame this set is already lexicographically sorted
(define (get-grouped-by gdf)
  (for/vector ([iv (in-vector (first (grouped-data-frame-group-indices gdf)))])
    (apply df-ref*
           (grouped-data-frame-delegate-frame gdf)
           (ivl-beg iv)
           (reverse (grouped-data-frame-groups gdf)))))
