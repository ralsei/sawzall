#lang racket
(require rackunit
         sawzall
         "test-data.rkt"
         "util.rkt")

;; these really should be provided by sawzall, tbh
(define (v/ vec c) (vector-map (λ (v) (/ v c)) vec))
(define (sum vec) (for/sum ([v (in-vector vec)]) v))

;; simple element mapping
(define create-1 (create docs1 [egrp (grp) (string-append "e" grp)]))
(define create-1-result
  (column-df [grp #("a" "a" "b" "b" "b")]
             [egrp #("ea" "ea" "eb" "eb" "eb")]
             [trt #("a" "b" "a" "b" "b")]
             [adult #(1 2 3 4 5)]
             [juv #(10 20 30 40 50)]))

;; overwriting an existing column
(define create-2 (create docs1 [grp (grp) (string-append "e" grp)]))
(define create-2-result
  (column-df [grp #("ea" "ea" "eb" "eb" "eb")]
             [trt #("a" "b" "a" "b" "b")]
             [adult #(1 2 3 4 5)]
             [juv #(10 20 30 40 50)]))

;; using a vector as an input, and returning a vector as input
(define create-3 (create docs1 [norm-juv ([juv : vector]) (v/ juv (sum juv))]))
(define create-3-result
  (column-df [grp #("a" "a" "b" "b" "b")]
             [trt #("a" "b" "a" "b" "b")]
             [adult #(1 2 3 4 5)]
             [juv #(10 20 30 40 50)]
             [norm-juv #(1/15 2/15 1/5 4/15 1/3)]))

;; using a vector and a scalar as input, and mapping over, keeping the vector constant
(define create-4
  (create docs1 [what ([adult : element] [juv : vector]) (/ adult (sum juv))]))
(define create-4-result
  (column-df [grp #("a" "a" "b" "b" "b")]
             [trt #("a" "b" "a" "b" "b")]
             [adult #(1 2 3 4 5)]
             [juv #(10 20 30 40 50)]
             [what #(1/150 1/75 1/50 2/75 1/30)]))

;; multiple clauses, mixing up all of the above
(define create-5
  (create docs1
          [grp (grp) (string-append "e" grp)]
          [norm-juv ([juv : vector]) (v/ juv (sum juv))]
          [what ([adult : element] [juv : vector]) (/ adult (sum juv))]))
(define create-5-result
  (column-df [grp #("ea" "ea" "eb" "eb" "eb")]
             [trt #("a" "b" "a" "b" "b")]
             [adult #(1 2 3 4 5)]
             [juv #(10 20 30 40 50)]
             [norm-juv #(1/15 2/15 1/5 4/15 1/3)]
             [what #(1/150 1/75 1/50 2/75 1/30)]))

;; clauses can depend on ones before them
(define create-6
  (create docs1
          [grp (grp) (string-append "e" grp)]
          [trt (trt grp) (string-append trt grp)]))
(define create-6-result
  (column-df [grp #("ea" "ea" "eb" "eb" "eb")]
             [trt #("aea" "bea" "aeb" "beb" "beb")]
             [adult #(1 2 3 4 5)]
             [juv #(10 20 30 40 50)]))

(module+ test
  (check data-frame~=? create-1 create-1-result)
  (check data-frame~=? create-2 create-2-result)
  (check data-frame~=? create-3 create-3-result)
  (check data-frame~=? create-4 create-4-result)
  (check data-frame~=? create-5 create-5-result)
  (check data-frame~=? create-6 create-6-result))
