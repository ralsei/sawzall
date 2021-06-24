#lang racket/base
(require data-frame
         text-table)
(provide show introspect)

; this should probably be more sophisticated. but it's fine for now
(define (show df)
  (print-table
   (let ([series (df-series-names df)])
     (cons series
           (for/list ([v (apply in-data-frame/list df series)])
             v)))))

(define (introspect df)
  (show df) df)
