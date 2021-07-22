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

(define gss-sm (df-read/csv "data/gss_sm.csv"))
(define organdata (df-read/csv "data/organdata.csv"))
(define iris (df-read/csv "data/iris.csv"))
