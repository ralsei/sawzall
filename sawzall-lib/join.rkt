#lang racket/base
(require data-frame
         fancy-app
         "helpers.rkt"
         "grouping.rkt"
         "reorder.rkt"
         "syntax.rkt")
(provide left-join right-join)

(define (left-join df1 df2 by #:cmp? [cmp? orderable<?])
  (ignore-grouping (left-join/int _ (ungroup-all df2) by cmp?) df1))
(define (right-join df1 df2 by #:cmp? [cmp? orderable<?])
  (ignore-grouping (left-join/int (ungroup-all df2) _ by cmp?) df1))

; takes all rows from df1, and all columns from df1 and df2. rows in df1 with no match in df2
; will have #f values in the new columns.
;
; XXX: is there a way to determine what the actual NA value is, or do we just have to guess?
(define (left-join/int df1 df2 by cmp?)
  (define return-df (make-data-frame))

  (define df1-sorted (reorder/int df1 (sort-proc (list by) (list cmp?))))
  (define df2-sorted (reorder/int df2 (sort-proc (list by) (list cmp?))))

  ; the return of merge from the hit series mergesort
  (define index-pairs
    (let loop ([df1-idx 0] [df2-idx 0] [pairs '()])
      (cond [(>= df1-idx (df-row-count df1-sorted)) (reverse pairs)]
            [(>= df2-idx (df-row-count df2-sorted))
             (loop (add1 df1-idx) df2-idx (cons (cons df1-idx #f) pairs))]
            [(equal? (df-ref df1-sorted df1-idx by) (df-ref df2-sorted df2-idx by))
             (loop (add1 df1-idx) (add1 df2-idx) (cons (cons df1-idx df2-idx) pairs))]
            [(cmp? (df-ref df1-sorted df1-idx by) (df-ref df2-sorted df2-idx by))
             (loop (add1 df1-idx) df2-idx (cons (cons df1-idx #f) pairs))]
            [else (loop df1-idx (add1 df2-idx) pairs)])))

  (for ([name (in-list (df-series-names df2-sorted))]
        #:when (not (equal? name by)))
    (df-add-series!
     return-df
     (make-series name #:data (for/vector ([idx (in-list (map cdr index-pairs))])
                                (if idx (df-ref df2-sorted idx name) #f)))))
  (for ([name (in-list (df-series-names df1-sorted))])
    (df-add-series!
     return-df
     (make-series name #:data (for/vector ([idx (in-list (map car index-pairs))])
                                (df-ref df1-sorted idx name)))))
  return-df)

(define df1 (make-data-frame))
(define df2 (make-data-frame))

(df-add-series! df1 (make-series "site" #:data (vector "b" "a" "c")))
(df-add-series! df1 (make-series "habitat" #:data (vector "grassland" "meadow" "woodland")))

(df-add-series! df2 (make-series "site" #:data (vector "c" "b" "c" "b")))
(df-add-series! df2 (make-series "day" #:data (vector 1 1 2 2)))
(df-add-series! df2 (make-series "catch" #:data (vector 10 12 20 24)))
