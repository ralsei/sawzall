#lang racket/base
(require data-frame
         fancy-app
         text-table
         racket/contract/base
         racket/list
         "grouped-df.rkt"
         "grouping.rkt")
(provide (contract-out [show (->* ((or/c data-frame? grouped-data-frame?))
                                  (#:all? boolean?)
                                  void?)]
                       [introspect (->* ((or/c data-frame? grouped-data-frame?))
                                        (#:all? boolean?)
                                        (or/c data-frame? grouped-data-frame?))]))

(define *show-rows-default* 6)
(define *show-cols-default* 6)

(define (show df #:all? [all? #f])
  (void (ignore-groups-apply (show-internal all?) df #:pass-groups? #t #:regroup? #f)))

(define ((show-internal all?) df grps)
  (define all-series (df-series-names df))
  (define n-rows (df-row-count df))
  (define n-cols (length all-series))
  (define row-cap (min *show-rows-default* n-rows))
  (define col-cap (min *show-cols-default* n-cols))

  (printf "data-frame: ~a rows x ~a columns~n" n-rows n-cols)
  (when (not (null? grps))
    (printf "groups: ~a~n" grps))

  (print-table
   (let ([series (if all? all-series (take all-series col-cap))])
     (cons series
           (for/list ([v (apply in-data-frame/list df series)]
                      [_ (if all? n-rows row-cap)])
             v))))

  (when (not (or all? (= row-cap n-rows) (= col-cap n-cols)))
    (printf "~a rows, ~a cols elided (use #:all? for full frame)~n"
            (- n-rows row-cap)
            (- n-cols col-cap))))

(define (introspect df #:all? [all? #f])
  (show df #:all? all?) df)
