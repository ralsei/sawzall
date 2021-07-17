#lang racket
(require racket/runtime-path
         rackunit
         sawzall
         threading
         "test-data.rkt"
         "util.rkt")

;; sorting by one string
(define reorder-1 (reorder woodland1 "site"))
(define reorder-1-result
  (row-df [site habitat]
          "a"   "meadow"
          "b"   "grassland"
          "c"   "woodland"))

;; sorting by two things
;; total ordering for first, ordering within for second
(define reorder-2 (reorder woodland2 "site" "day"))
(define reorder-2-result
  (row-df [site day catch]
          "b"   1   12
          "b"   2   24
          "c"   1   10
          "c"   2   20))

;; sorting by two things, with a custom comparator
(define reorder-3 (reorder woodland2 "site" (cons "day" >)))
(define reorder-3-result
  (row-df [site day catch]
          "b"   2   24
          "b"   1   12
          "c"   2   20
          "c"   1   10))

;; sorting with groups
(define reorder-4
  (~> woodland2
      (group-with "site")
      (reorder "day")
      ungroup))
(define reorder-4-result
  (row-df [site day catch]
          "b"   1   12
          "b"   2   24
          "c"   1   10
          "c"   2   20))

;; sorting gss-sm
(define-runtime-path reorder-gss-1-data "./results/reorder_gss_1.csv")
(define reorder-gss-1 (reorder gss-sm "bigregion"))

(define-runtime-path reorder-gss-2-data "./results/reorder_gss_2.csv")
(define reorder-gss-2 (reorder gss-sm "bigregion" "religion"))

(define-runtime-path reorder-gss-3-data "./results/reorder_gss_3.csv")
(define reorder-gss-3
  (~> gss-sm
      (group-with "bigregion" "religion")
      (reorder "obama")
      ungroup))

(module+ test
  (check data-frame~=? reorder-1 reorder-1-result)
  (check data-frame~=? reorder-2 reorder-2-result)
  (check data-frame~=? reorder-3 reorder-3-result)
  (check data-frame~=? reorder-4 reorder-4-result)

  (check df-sorted-by? reorder-gss-1 "bigregion")
  (check-csv reorder-gss-1 reorder-gss-1-data)
  (check df-sorted-by? reorder-gss-2 "bigregion")
  (check-csv reorder-gss-2 reorder-gss-2-data)
  (check-csv reorder-gss-3 reorder-gss-3-data))
