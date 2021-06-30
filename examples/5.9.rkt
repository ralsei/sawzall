#lang racket
(require data-frame math/statistics graphite "../main.rkt")

(define organdata (df-read/csv "data/organdata.csv" #:na "NA"))
(define sorted
  (reorder organdata
           [country (λ (a b)
                      (for/fold ([as null] [bs null] #:result (< (mean as) (mean bs)))
                                ([(val to-mean) (in-data-frame organdata "country" "donors")]
                                 #:when to-mean)
                        (cond [(equal? val a) (values (cons to-mean as) bs)]
                              [(equal? val b) (values as (cons to-mean bs))]
                              [else (values as bs)])))]))

(graph #:data sorted
       #:mapping (aes #:x "donors" #:y "country")
       #:width 700
       (boxplot #:invert? #t))
