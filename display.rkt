#lang racket/base
(require data-frame
         text-table
         "grouping.rkt")
(provide show introspect)

(define (show df)
  (void (group-map show-internal df #:pass-groups? #t)))

(define (show-internal df grps)
  (when (not (null? grps))
    (printf "groups: ~a~n" grps))
  (print-table
   (let ([series (df-series-names df)])
     (cons series
           (for/list ([v (apply in-data-frame/list df series)])
             v)))))

(define (introspect df)
  (show df) df)
