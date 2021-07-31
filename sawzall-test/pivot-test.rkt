#lang racket
(require data-frame
         racket/runtime-path
         rackunit
         sawzall
         threading
         "test-data.rkt"
         "util.rkt")

(define wide-df
  (row-df [day hour a  b  c]
           1   10   97 84 55
           2   11   78 47 54))

(define pivot-longer-1 (pivot-longer wide-df ["a" "b" "c"] #:names-to "site" #:values-to "catch"))
(define pivot-longer-1-result
  (row-df [day hour site catch]
          1    10   "a"  97
          2    11   "a"  78
          1    10   "b"  84
          2    11   "b"  47
          1    10   "c"  55
          2    11   "c"  54))

(define-runtime-path relig-income-pivot-longer-1-data
  "./results/relig_income_pivot_longer_1.csv")
(define relig-income-pivot-longer-1
  (pivot-longer relig-income (not "religion") #:names-to "income" #:values-to "count"))

(define-runtime-path billboard-pivot-longer-1-data
  "./results/billboard_pivot_longer_1.csv")
(define billboard-pivot-longer-1
  (~> billboard
      (pivot-longer (starting-with "wk")
                    #:names-to "week" #:values-to "rank")
      (create [week (week) (string->number (string-replace week "wk" ""))])))

(define long-df1
  (row-df [day grp val]
          1    "A" 10
          1    "B" 20
          2    "B" 30))

(define long-df2
  (row-df [day hour grp val]
          1    10   "a" 83
          1    10   "b" 78
          1    11   "a" 80
          1    11   "b" 105
          2    10   "a" 95
          2    10   "b" 77
          2    11   "a" 96
          2    11   "b" 99))

(define pivot-wider-1 (pivot-wider long-df1 #:names-from "grp" #:values-from "val"))
(define pivot-wider-1-result
  (row-df [day A  B]
          1    10 20
          2    #f 30))

(define pivot-wider-2 (pivot-wider long-df2 #:names-from "grp" #:values-from "val"))
(define pivot-wider-2-result
  (row-df [day hour a  b]
          1    10   83 78
          1    11   80 105
          2    10   95 77
          2    11   96 99))

(module+ test
  (check data-frame~=? pivot-longer-1 pivot-longer-1-result)

  (check-csv relig-income-pivot-longer-1 relig-income-pivot-longer-1-data)
  (check-csv billboard-pivot-longer-1 billboard-pivot-longer-1-data)
  (check-true (for/and ([v (in-data-frame billboard-pivot-longer-1 "week")])
                (number? v)))

  (check data-frame~=? pivot-wider-1 pivot-wider-1-result)
  (check data-frame~=? pivot-wider-2 pivot-wider-2-result))
