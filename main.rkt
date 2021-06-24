#lang racket/base
(require data-frame
         plot/utils
         threading
         "aggregate.rkt"
         "create.rkt"
         "display.rkt"
         "facet.rkt"
         "saw-lambda.rkt")

(define (sum vec)
  (for/sum ([v (in-vector vec)] #:when (number? v)) v))

; entry-point
(define gss-sm (df-read/csv "data/gss_sm.csv"))
; group by bigregion and religion, make a new column total that is
; the total of each religion in each region, _remove other columns_
(define aggregated
  (aggregate gss-sm
             '("bigregion" "religion")
             (saw-λ [total (religion) (vector-length religion)]))) ; argument is a vector here
; split along religion
(define faceted (facet aggregated "bigregion"))
; for each split, make a new table with the frequency of each religion's occurrence
; in that region
(define with-frequencies
  (map (λ (df)
         (create df (saw-λ [frequency (total) (/ total (sum (df-select df "total")))]
                           [percent (frequency) (round (* frequency 100))])))
       faceted))
; then combine them back together
(define with-frequency (apply unfacet with-frequencies))
; and let 'er rip
(show with-frequency)

; alternatively
(~> gss-sm
    (aggregate '("bigregion" "religion")
               (saw-λ [total (religion) (vector-length religion)]))
    (facet "bigregion")
    (map (λ (df)
           (~> df
               (create-all (saw-λ [frequency (total) (v/ total (sum total))]))
               (create (saw-λ [percent (frequency) (round (* frequency 100))]))))
         _)
    (apply unfacet _)
    show)

; alternatively alternatively??
;; (~> gss-sm
;;     (group-with "bigregion" "religion") ; this turns into a data-frame + grouping info
;;     (aggregate (saw-λ [total (religion) (vector-length religion)]))
;;     (create-all (saw-λ [frequency (total) (v/ total (sum total))]))
;;     (create (saw-λ [percent (frequency) (round (* frequency 100))]))
;;     ungroup ; turns back into a data-frame
;;     show)
