#lang racket
(require data-frame graphite "../main.rkt")

(define organdata (df-read/csv "data/organdata.csv"))
(define sorted
  (reorder organdata (sort-with "country" (mean-of "donors"))))
