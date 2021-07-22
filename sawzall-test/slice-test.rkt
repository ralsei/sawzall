#lang racket
(require data-frame
         racket/runtime-path
         rackunit
         sawzall
         threading
         "test-data.rkt"
         "util.rkt")

;; single variable
(define slice-1 (slice woodland1 "site"))
(define slice-1-result
  (column-df [site #("b" "a" "c")]))

;; multiple variables
(define slice-2 (slice woodland1 ["site" "habitat"]))
(define slice-2-result woodland1)

(define slice-3 (slice ball2 (starting-with "g")))
(define slice-3-result
  (column-df [game #(1 1 1 2)]
             [goals #(0 1 2 3)]))

(define slice-4 (slice ball2 (ending-with "t")))
(define slice-4-result
  (column-df [first #("sam" "bob" "dan" "bob")]
             [last #("son" "ert" "man" "ert")]))

;; union
(define slice-5 (slice ball2 (or (starting-with "g") (ending-with "t"))))
(define slice-5-result ball2)

;; intersection
(define slice-6 (slice ball2 (and (starting-with "g") (ending-with "t"))))
(define slice-6-result (make-data-frame))

;; basic regex
(define slice-7 (slice docs1 #px"r"))
(define slice-7-result
  (column-df [grp #("a" "a" "b" "b" "b")]
             [trt #("a" "b" "a" "b" "b")]))

;; slicing up iris, as in dplyr::select examples
(define-runtime-path slice-iris-1-data "./results/slice_iris_1.csv")
(define slice-iris-1 (slice iris (not ["Sepal.Length" "Petal.Length"])))

(define-runtime-path slice-iris-2-data "./results/slice_iris_2.csv")
(define slice-iris-2 (slice iris (not (ending-with "Width"))))

(define-runtime-path slice-iris-3-data "./results/slice_iris_3.csv")
(define slice-iris-3 (slice iris (and (starting-with "Petal") (not (ending-with "Width")))))

(module+ test
  ;; cannot remove a group variable from a grouped data frame
  (check-exn exn? (thunk (~> woodland2
                             (group-with "day")
                             (slice (not "day"))
                             ungroup)))
  ;; ensure that these doesn't work despite grammar
  (check-exn exn? (thunk (slice woodland1 "not-in-woodland1")))
  ;; this is valid syntax because `starts-with` could be an (expr/c #'string?),
  ;; but we should runtime error on it
  (check-exn exn? (thunk (slice woodland1 (starts-with "bleh"))))

  (check data-frame~=? slice-1 slice-1-result)
  (check data-frame~=? slice-2 slice-2-result)
  (check data-frame~=? slice-3 slice-3-result)
  (check data-frame~=? slice-4 slice-4-result)
  (check data-frame~=? slice-5 slice-5-result)
  (check data-frame~=? slice-6 slice-6-result)
  (check data-frame~=? slice-7 slice-7-result)

  (check-csv slice-iris-1 slice-iris-1-data)
  (check-equal? (sort (df-series-names slice-iris-1) string-ci<?)
                (list "#f" "Petal.Width" "Sepal.Width" "Species"))
  (check-csv slice-iris-2 slice-iris-2-data)
  (check-equal? (sort (df-series-names slice-iris-2) string-ci<?)
                (list "#f" "Petal.Length" "Sepal.Length" "Species"))
  (check-csv slice-iris-3 slice-iris-3-data)
  (check-equal? (sort (df-series-names slice-iris-3) string-ci<?)
                (list "Petal.Length")))
