#lang racket
(require data-frame
         graphite
         sawzall
         threading)

;; read in some dirty data
(define who-dirty (df-read/csv "data/who.csv" #:na "NA"))
;; bug (?) in df-read/csv
(df-del-series! who-dirty "")

(define who
  (~> who-dirty
      ;; give it a generic name, because we don't know what these values
      ;; mean yet. everything else is of the form new_rel or newrel or something
      ;; like that.
      (pivot-longer (not ["country" "iso2" "iso3" "year"])
                    #:names-to "key" #:values-to "cases")
      ;; NA values probably aren't important.
      (drop-na "cases")
      ;; annoyingly, we have newrel in some keys aside from new_rel, so replace
      ;; those
      (create [key (key) (string-replace key "newrel" "new_rel")])
      ;; split up the keys into something more meaningful
      (separate "key"
                #:into '("new" "type" "sex-age")
                #:separator "_")
      ;; drop useless columns
      (slice (not ["new" "iso2" "iso3"]))
      ;; separate "sex-age" into sex and age on the first character
      (separate "sex-age"
                #:into '("sex" "age")
                #:separator 1)))

(~> who
    (where* (country) ("Afghanistan"))
    (group-with "year" "age")
    (aggregate [cases-sum (cases) (for/sum ([v (in-vector cases)]) v)])
    ungroup
    (graph #:data _
           #:mapping (aes #:x "year" #:y "cases-sum" #:discrete-color "age")
           (lines)))
