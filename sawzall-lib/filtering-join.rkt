#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         "generic-join.rkt"
         "grouped-df.rkt"
         "grouping.rkt")
(provide (contract-out [semi-join (-> (or/c data-frame? grouped-data-frame?)
                                      (or/c data-frame? grouped-data-frame?)
                                      string? ...
                                      (or/c data-frame? grouped-data-frame?))]
                       [anti-join (-> (or/c data-frame? grouped-data-frame?)
                                      (or/c data-frame? grouped-data-frame?)
                                      string? ...
                                      (or/c data-frame? grouped-data-frame?))]))

(define (semi-join df1 df2 . by)
  (ignore-groups-apply (semi-join-dfs _ (ungroup df2) by) df1))
(define (anti-join df1 df2 . by)
  (ignore-groups-apply (anti-join-dfs _ (ungroup df2) by) df1))

(define (join-matches df1 df2 by)
  (define df1-int (sub-data-frame-delegate-frame df1))

  (define return-df (make-data-frame))
  (for ([name (in-list (df-series-names df1-int))])
    (df-add-series! return-df (make-series name #:data (df-select/sub df1 name))))

  return-df)

(define (join-no-matches df1 df2-series)
  (define df1-int (sub-data-frame-delegate-frame df1))

  (define return-df (make-data-frame))
  (for ([name (in-list (df-series-names df1-int))])
    (df-add-series! return-df (make-series name #:data (df-select/sub df1 name))))

  return-df)

(define semi-join-dfs
  (generic-join
   #:on-= (λ (df1 df2 by acc) (cons (join-matches df1 df2 by) acc))
   #:on-end (λ (df1 df2-names acc) acc)
   #:on-< (λ (df1 df2-names acc) acc)
   #:on-> (λ (df1-names df2 acc) acc)))

(define anti-join-dfs
  (generic-join
   #:on-= (λ (df1 df2 by acc) acc)
   #:on-end (λ (df1 df2-names acc) (cons (join-no-matches df1 df2-names) acc))
   #:on-< (λ (df1 df2-names acc) (cons (join-no-matches df1 df2-names) acc))
   #:on-> (λ (df1-names df2 acc) acc)))
