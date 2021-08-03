#lang racket
(require graphite
         sawzall
         threading)

;; anscombe's quartet, from R
;; each "x1" "y1" pair has extremely similar summary statistics
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

;; this data is fine, but we want to show everything with one graphite facet!
;; what we want is a variable to facet on... but we need to induce one first...
(define facetable
  (~> anscombe
      ;; create a column so we don't have any duplicates, and pivot-wider knows
      ;; what to bind on
      ;;
      ;; this could be better -- without this it errors silently, at least right now.
      (create [nrow ([x1 : vector]) (build-vector (vector-length x1) (λ (x) x))])
      ;; take every column but nrow,
      ;; names to a new column called "name" and the values to a new column called "val"
      (pivot-longer (not "nrow") #:names-to "name" #:values-to "val")
      ;; take "name", which is comprised of "x1" "x2" "y1" et al,
      ;; and turn it into two columns by splitting on the first character,
      ;; one representing the variable and the other representing the quadrant
      (separate "name" #:into '("x-or-y" "quadrant") #:separator 1)
      ;; take the column "x-or-y", and take all the "x" values and make them a column,
      ;; then the "y" values and make them a column
      (pivot-wider #:names-from "x-or-y" #:values-from "val")
      ;; remove the column we built at the start
      (slice (not "nrow"))))

;; so the data now looks like:
;; ┌──┬────┬────────┐
;; │x │y   │quadrant│
;; ├──┼────┼────────┤
;; │10│8.04│1       │
;; ├──┼────┼────────┤
;; │8 │6.95│1       │
;; ├──┼────┼────────┤
;; │13│7.58│1       │
;; ├──┼────┼────────┤
;; │9 │8.81│1       │
;; ├──┼────┼────────┤
;; │11│8.33│1       │
;; ├──┼────┼────────┤
;; │14│9.96│1       │
;; └──┴────┴────────┘
;; so we have a variable we can facet on!

;; read in "facetable",
(graph #:data facetable
       ;; use "x" for x and "y" for y, facet on "quadrant",
       #:mapping (aes #:x "x" #:y "y" #:facet "quadrant")
       #:width 800 #:height 800
       ;; and draw points and a linear fit line
       (points)
       (fit #:width 3))
