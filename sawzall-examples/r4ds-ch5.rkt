#lang racket
(require colormaps
         data-frame
         file/gunzip
         math/statistics
         graphite
         sawzall
         threading)

(require profile)

;; load in data from the R nycflights13 package
;; R's `write.csv` stores NA as a string, so we have to account for that
(define airlines (df-read/csv "data/airlines.csv" #:na "NA"))
(define airports (df-read/csv "data/airports.csv" #:na "NA"))
(define planes (df-read/csv "data/planes.csv" #:na "NA"))
(define weather (df-read/csv "data/weather.csv" #:na "NA"))
;; flights is really big, so it's stored gzipped
(define flights
  (let ()
    (define data
      (call-with-output-string
       (λ (out)
         (call-with-input-file "data/flights.csv.gz"
           (λ (in) (gunzip-through-ports in out))))))
    (call-with-input-string data (curry df-read/csv #:na "NA"))))

;; only january 1st
;; cpu time: 163 real time: 164 gc time: 37
;; (define jan1 (where flights (month day) (and (= month 1) (= day 1))))
;; only december 25th
;; cpu time: 113 real time: 113 gc time: 0
;; (define dec25 (where flights (month day) (and (= month 12) (= day 25))))
;; november or december
;; you can use arbitrary boolean expressions here!
;; cpu time: 984 real time: 985 gc time: 90
;; (define nov-dec (where flights (month) (or (= month 11) (= month 12))))
;; not delayed by more than 2 hours
;; these values could be NA, and we have to account for that
;; cpu time: 5869 real time: 5874 gc time: 321
;; (define roughly-on-time
;;   (where flights (arr_delay dep_delay)
;;          (or (and arr_delay (<= arr_delay 120))
;;              (and dep_delay (<= dep_delay 120)))))

;; sort by year, then month, then day
;; cpu time: 3937 real time: 3964 gc time: 1188
;; (define date-sorted
;;   (reorder flights "year" "month" "day"))
;; sort by departure delay, descending
;; use a makeshift orderable>=?, we have to account for NA again
;; cpu time: 2033 real time: 2035 gc time: 194
;; (define desc-dep-delay
;;   (reorder flights (cons "dep_delay" (λ (a b)
;;                                        (or (equal? a b)
;;                                            (not (orderable<? a b)))))))

;; select() doesn't exist here. we just have df-select

;; make new columns
;; again, accounting for NA
;; cpu time: 131 real time: 131 gc time: 40
;; (define dunno
;;   (create flights
;;           [gain (dep_delay arr_delay) (and dep_delay arr_delay (- dep_delay arr_delay))]
;;           [speed (distance air_time) (and distance air_time (* (/ distance air_time) 60))]))

;; modular arithmetic
;; cpu time: 59 real time: 60 gc time: 26
;; (define dunno-2
;;   (create flights
;;           [hour (dep_time) (and dep_time (quotient dep_time 100))]
;;           [minute (dep_time) (and dep_time (remainder dep_time 100))]))

;; collapsing into a single row
;; cpu time: 89 real time: 89 gc time: 30
;; (define almost-useless
;;   (aggregate flights [delay (dep_delay) (mean (vector-filter identity dep_delay))]))

;; grouping stuff up then aggregating retains the groups
;; splitting. uh. takes a while, apparently?
;; cpu time: 7126 real time: 7181 gc time: 1896
;; (define delay-by-day
;;   (~> flights
;;       (group-with "year" "month" "day")
;;       (aggregate [delay (dep_delay) (mean (vector-filter identity dep_delay))])
;;       ungroup))

;; exploring the relationship between the distance and average delay for each location
;; note the implicit ungroup for aggregate
;; cpu time: 11113 real time: 11133 gc time: 906

(profile-thunk
 (λ ()
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
          (fit #:method 'loess #:width 3))))
