#lang racket
(require data-frame sawzall)
(provide (all-defined-out))

(define woodland1 (make-data-frame))
(define woodland2 (make-data-frame))

(df-add-series! woodland1 (make-series "site" #:data (vector "b" "a" "c")))
(df-add-series! woodland1 (make-series "habitat" #:data (vector "grassland" "meadow" "woodland")))

(df-add-series! woodland2 (make-series "site" #:data (vector "c" "b" "c" "b")))
(df-add-series! woodland2 (make-series "day" #:data (vector 1 1 2 2)))
(df-add-series! woodland2 (make-series "catch" #:data (vector 10 12 20 24)))

(define ball1 (make-data-frame))
(define ball2 (make-data-frame))

(df-add-series! ball1 (make-series "first" #:data (vector "sam" "bob" "sam" "dan")))
(df-add-series! ball1 (make-series "last" #:data (vector "son" "ert" "jam" "man")))
(df-add-series! ball1 (make-series "age" #:data (vector 10 20 30 40)))

(df-add-series! ball2 (make-series "first" #:data (vector "sam" "bob" "dan" "bob")))
(df-add-series! ball2 (make-series "last" #:data (vector "son" "ert" "man" "ert")))
(df-add-series! ball2 (make-series "game" #:data (vector 1 1 1 2)))
(df-add-series! ball2 (make-series "goals" #:data (vector 0 1 2 3)))

(define docs1
  (column-df [grp #("a" "a" "b" "b" "b")]
             [trt #("a" "b" "a" "b" "b")]
             [adult #(1 2 3 4 5)]
             [juv #(10 20 30 40 50)]))

(define anscombe
  (row-df [x1 x2 x3 x4 y1    y2   y3    y4]
          10  10 10 8  8.04  9.14 7.46  6.58
          8   8  8  8  6.95  8.14 6.77  5.76
          13  13 13 8  7.58  8.74 12.74 7.71
          9   9  9  8  8.81  8.77 7.11  8.84
          11  11 11 8  8.33  9.26 7.81  8.47
          14  14 14 8  9.96  8.10 8.84  7.04
          6   6  6  8  7.24  6.13 6.08  5.25
          4   4  4  19 4.26  3.10 5.39  12.50
          12  12 12 8  10.84 9.13 8.15  5.56
          7   7  7  8  4.82  7.26 6.42  7.91
          5   5  5  8  5.68  4.74 5.73  6.89))

(define gss-sm (df-read/csv "data/gss_sm.csv"))
(define organdata (df-read/csv "data/organdata.csv"))
(define iris (df-read/csv "data/iris.csv"))
(define relig-income (df-read/csv "data/relig_income.csv"))
(define billboard (df-read/csv "data/billboard.csv"))
