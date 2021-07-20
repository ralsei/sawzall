#lang racket
(require data-frame
         racket/runtime-path
         rackunit
         sawzall
         threading
         "test-data.rkt"
         "util.rkt")

;; aggregate to a scalar
(define aggregate-1 (aggregate woodland1 [N (site) (vector-length site)]))
(define aggregate-1-result
  (row-df [N]
          3))

(define aggregate-2
  (aggregate woodland1
             [combined (habitat)
                       (for/fold ([str ""])
                                 ([h (in-vector habitat)])
                         (string-append str " " h))]))
(define aggregate-2-result
  (row-df [combined]
          " grassland meadow woodland"))

;; aggregating with groups
(define (sum vec) (for/sum ([v (in-vector vec)] #:when v) v))

;; implicit ungroup
(define aggregate-3
  (~> docs1
      (group-with "grp")
      (aggregate [adult-sum (adult) (sum adult)]
                 [juv-sum (juv) (sum juv)])))
(define aggregate-3-result
  (row-df [grp adult-sum juv-sum]
          "a"  3         30
          "b"  12        120))

;; needs explicit ungroup
(define aggregate-4
  (~> docs1
      (group-with "grp" "trt")
      (aggregate [adult-sum (adult) (sum adult)]
                 [juv-sum (juv) (sum juv)])
      ungroup))
(define aggregate-4-result
  (row-df [grp trt adult-sum juv-sum]
          "a"  "a" 1         10
          "a"  "b" 2         20
          "b"  "a" 3         30
          "b"  "b" 9         90))

;; gss-sm aggregation
(define-runtime-path aggregate-gss-1-data "./results/aggregate_gss_1.csv")
(define aggregate-gss-1
  (~> gss-sm
      (group-with "bigregion" "religion")
      (aggregate [N (religion) (vector-length religion)])
      ungroup))

(module+ test
  (check data-frame~=? aggregate-1 aggregate-1-result)
  (check data-frame~=? aggregate-2 aggregate-2-result)
  (check data-frame~=? aggregate-3 aggregate-3-result)
  (check data-frame~=? aggregate-4 aggregate-4-result)

  (check-csv aggregate-gss-1 aggregate-gss-1-data))
