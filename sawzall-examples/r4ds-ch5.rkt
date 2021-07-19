#lang racket
(require colormaps
         data-frame
         file/gunzip
         math/statistics
         graphite
         sawzall
         threading)
(provide (all-defined-out))

(require profile)

;; load in data from the R nycflights13 package
;; really big, so it's stored gzipped
(define flights
  (let ()
    (define data
      (call-with-output-string
       (λ (out)
         (call-with-input-file "data/flights.csv.gz"
           (λ (in) (gunzip-through-ports in out))))))
    (call-with-input-string data (curry df-read/csv #:na "NA"))))

;; exploring the relationship between the distance and average delay for each location
;; note the implicit ungroup for aggregate
(define delays
  (~> flights
      (group-with "dest")
      (aggregate [count (dep_delay) (vector-length dep_delay)]
                 [dist (distance) (mean (vector-filter identity distance))]
                 [delay (arr_delay) (mean (vector-filter identity arr_delay))])
      (where (count dest) (and (> count 20) (not (equal? dest "HNL"))))))

(graph #:data delays
       #:mapping (aes #:x "dist" #:y "delay")
       #:theme (theme-override theme-default #:color-map 'tol-sd)
       (points #:mapping (aes #:continuous-color "count"))
       (fit #:method 'loess #:width 3))

;; get all the flights that haven't been cancelled (which is a NA delay)
(define not-cancelled
  (~> flights
      (where (arr_delay dep_delay) (and arr_delay dep_delay))))

;; planes identified by tail number with the highest average delays
;; filter out groups with small numbers of observations, to get to more trends
(define delays-by-tailnum
  (~> not-cancelled
      (group-with "tailnum")
      (aggregate [delay (arr_delay) (mean arr_delay)]
                 [N (arr_delay) (vector-length arr_delay)])
      (where (N) (> N 25))))

;; we don't have geom_freqpoly() in graphite. maybe we should?
(graph #:data delays-by-tailnum
       #:mapping (aes #:x "N" #:y "delay")
       #:x-min -2 #:width 700
       (points #:alpha 1/10 #:color "black"))
