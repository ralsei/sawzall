#lang racket/base
(require data-frame
         fancy-app
         text-table
         racket/list
         "grouping.rkt")
(provide show introspect)

(define *show-rows-default* 6)
(define *show-cols-default* 6)

(define (show df #:all? [all? #f])
  (void (group-map (show-internal _ _ all?) df #:pass-groups? #t)))

(define (show-internal df grps all?)
  (define all-series (df-series-names df))
  (define n-rows (df-row-count df))
  (define n-cols (length all-series))
  (define row-cap (min *show-rows-default* n-rows))
  (define col-cap (min *show-cols-default* n-cols))

  (printf "data-frame: ~a rows x ~a columns~n" n-rows n-cols)
  (when (not (null? grps))
    (printf "groups: ~a~n" grps))

  (print-table
   (let ([series (take all-series col-cap)])
     (cons series
           (for/list ([v (apply in-data-frame/list df series)]
                      [_ row-cap])
             v))))

  (when (not all?)
    (printf "~a rows, ~a cols elided (use #:all? for full frame)~n"
            (- n-rows row-cap)
            (- n-cols col-cap))))

(define (introspect df)
  (show df) df)
