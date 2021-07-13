#lang racket
(require data-frame math/statistics graphite threading sawzall)

(define organdata (df-read/csv "data/organdata.csv" #:na "NA"))
(define sorted-countries
  (~> organdata
      (group-with "country")
      (aggregate [med (donors) (median < (vector-filter identity donors))])
      (reorder "med")
      (df-select "country")))
(define sorted
  (~> organdata
      (reorder (cons "country" (by-vector sorted-countries)))))

(graph #:data sorted
       #:mapping (aes #:x "donors" #:y "country")
       #:width 700
       (boxplot #:invert? #t))
