#lang racket/base
(require data-frame
         text-table
         "grouping.rkt")
(provide show introspect)

; this should probably be more sophisticated. but it's fine for now
(define (show df)
  (printf "groups: ~a" (get-groups df))
  (for ([f (in-list (get-frames df))])
    (print-table
     (let ([series (df-series-names f)])
       (cons series
             (for/list ([v (apply in-data-frame/list f series)])
               v))))))

(define (introspect df)
  (show df) df)
