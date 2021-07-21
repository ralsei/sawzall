#lang racket
(require rackunit
         sawzall
         "test-data.rkt"
         "util.rkt")

(define left-join-1 (left-join woodland1 woodland2 "site"))
(define left-join-1-result
  (row-df [day catch site habitat]
          #f   #f    "a"  "meadow"
          1    12    "b"  "grassland"
          2    24    "b"  "grassland"
          1    10    "c"  "woodland"
          2    20    "c"  "woodland"))

(define left-join-2 (left-join woodland2 woodland1 "site"))
(define left-join-2-result
  (row-df [habitat    catch site day]
          "grassland" 12    "b"  1
          "grassland" 24    "b"  2
          "woodland"  10    "c"  1
          "woodland"  20    "c"  2))

(define left-join-3
  (left-join (row-df [key val-x]
                     1    "x1"
                     2    "x2"
                     2    "x3"
                     1    "x4")
             (row-df [key val-y]
                     1    "y1"
                     2    "y2")
             "key"))
(define left-join-3-result
  (row-df [key val-x val-y]
          1 "x1" "y1"
          1 "x4" "y1"
          2 "x2" "y2"
          2 "x3" "y2"))

(define left-join-4
  (left-join (row-df [key val-x]
                     1    "x1"
                     2    "x2"
                     2    "x3"
                     3    "x4")
             (row-df [key val-y]
                     1    "y1"
                     2    "y2"
                     2    "y3"
                     3    "y4")
             "key"))
(define left-join-4-result
  (row-df [key val-x val-y]
          1    "x1"  "y1"
          2    "x2"  "y2"
          2    "x2"  "y3"
          2    "x3"  "y2"
          2    "x3"  "y3"
          3    "x4"  "y4"))

;; join by multiple variables
(define left-join-5 (left-join ball1 ball2 "first" "last"))
(define left-join-5-result
  (row-df [first last  age game goals]
          "bob"  "ert" 20  1    1
          "bob"  "ert" 20  2    3
          "dan"  "man" 40  1    2
          "sam"  "jam" 30  #f   #f
          "sam"  "son" 10  1    0))

(define left-join-6 (left-join ball2 ball1 "first" "last"))
(define left-join-6a (left-join ball2 ball1))
(define left-join-6-result
  (row-df [first last  game goals age]
          "bob"  "ert" 1    1     20
          "bob"  "ert" 2    3     20
          "dan"  "man" 1    2     40
          "sam"  "son" 1    0     10))

(define right-join-1 (right-join woodland2 woodland1 "site"))
(define right-join-1-result left-join-1-result)

(define right-join-2 (right-join woodland1 woodland2 "site"))
(define right-join-2-result left-join-2-result)

(define inner-join-1 (inner-join woodland1 woodland2 "site"))
(define inner-join-1-result
  (row-df [day catch site habitat]
          1    12    "b"  "grassland"
          2    24    "b"  "grassland"
          1    10    "c"  "woodland"
          2    20    "c"  "woodland"))

(define inner-join-2
  (inner-join (row-df [key val-x]
                      1    "x1"
                      2    "x2"
                      3    "x3")
              (row-df [key val-y]
                      1    "y1"
                      2    "y2"
                      4    "y3")
              "key"))
(define inner-join-2-result
  (row-df [key val-x val-y]
          1    "x1"  "y1"
          2    "x2"  "y2"))

(define full-join-1 (full-join woodland2 woodland1 "site"))
(define full-join-1-result
  (row-df [day catch site habitat]
          #f   #f    "a"  "meadow"
          1    12    "b"  "grassland"
          2    24    "b"  "grassland"
          1    10    "c"  "woodland"
          2    20    "c"  "woodland"))

(module+ test
  (check data-frame~=? left-join-1 left-join-1-result)
  (check data-frame~=? left-join-2 left-join-2-result)
  (check data-frame~=? left-join-3 left-join-3-result)
  (check data-frame~=? left-join-4 left-join-4-result)
  (check data-frame~=? left-join-5 left-join-5-result)
  (check data-frame~=? left-join-6 left-join-6-result)
  (check data-frame~=? left-join-6a left-join-6-result)

  (check data-frame~=? right-join-1 right-join-1-result)
  (check data-frame~=? right-join-2 right-join-2-result)

  (check data-frame~=? inner-join-1 inner-join-1-result)
  (check data-frame~=? inner-join-2 inner-join-2-result)

  (check data-frame~=? full-join-1 full-join-1-result))
