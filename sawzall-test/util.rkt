#lang racket
(require data-frame sawzall)
(provide data-frame~=? df-sorted-by?)

; checks if two data-frames are "equivalent".
; conditions, checked sequentially:
; - they have the same series names
; - they have the same row count
; - they have the same data in each series (by equal?)
(define (data-frame~=? df1 df2)
  (and (apply df-contains? df1 (df-series-names df2))
       (apply df-contains? df2 (df-series-names df1))

       (= (df-row-count df1) (df-row-count df2))

       (for*/and ([name (in-list (df-series-names df1))]
                  [(val-df1 val-df2) (in-parallel (in-data-frame df1 name)
                                                  (in-data-frame df2 name))])
         (equal? val-df1 val-df2))))

; checks if a data-frame is sorted by the given column
(define (df-sorted-by? df by #:cmp? [cmp? orderable<?])
  (for/and ([tortoise (in-vector (df-select df by))]
            [hare (in-vector (df-select df by) 1)])
    (or (equal? tortoise hare)
        (cmp? tortoise hare))))

(module+ test
  (require rackunit)

  (define df1
    (for*/data-frame (al bl)
                     ([as (in-range 6)]
                      [bs (in-range 8)])
      (values as bs)))
  (define df2
    (for*/data-frame (bl al)
                     ([as (in-range 6)]
                      [bs (in-range 8)])
      (values bs as)))
  (check data-frame~=? df1 df2)
  (check df-sorted-by? df1 "al"))
