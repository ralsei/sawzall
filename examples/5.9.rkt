#lang racket
(require data-frame math/statistics graphite threading "../main.rkt")

(define organdata (df-read/csv "data/organdata.csv" #:na "NA"))
(define sorted-countries
  (~> organdata
      (group-with "country")
      (aggregate [mean (donors) (mean (vector-filter identity donors))])
      ungroup
      (reorder [mean <])
      (df-select "country")))
(define sorted
  (~> organdata
      (reorder [country (by-vector sorted-countries)])))

(graph #:data sorted
       #:mapping (aes #:x "donors" #:y "country")
       #:width 700
       (boxplot #:invert? #t))
