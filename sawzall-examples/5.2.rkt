#lang racket
(require data-frame
         graphite
         threading
         racket/vector
         sawzall)

(define (v/ vec c) (vector-map (λ (v) (/ v c)) vec))
(define (sum vec) (for/sum ([v (in-vector vec)] #:when (number? v)) v))

(define gss-sm (df-read/csv "data/gss_sm.csv"))

;; rel_by_region <- gss_sm %>%
;;     group_by(bigregion, religion) %>%
;;     summarize(N = n()) %>%
;;     mutate(freq = N / sum(N),
;;            pct = round((freq*100), 0))
(define rel-by-region
  (~> gss-sm
      (group-with "bigregion" "religion")
      (aggregate [N (religion) (vector-length religion)])
      introspect
      (create [freq ([N : vector]) (v/ N (sum N))]
              [pct (freq) (round (* freq 100))])
      ungroup))

;; ;; p <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion))
;; ;; p + geom_col(position = "dodge2") +
;; ;;     labs(x = NULL, y = "Percent", fill = "Religion") +
;; ;;     guides(fill = FALSE) +
;; ;;     coord_flip() +
;; ;;     facet_grid(~ bigregion)
(graph #:data rel-by-region
       #:mapping (aes #:x "religion" #:y "pct" #:facet "bigregion")
       #:facet-wrap 2
       #:title "Religious preferences by region"
       #:x-label "Religion" #:y-label "Percent"
       #:height 800
       #:width 800
       (col))
