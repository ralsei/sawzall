#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/list
         "generic-join.rkt"
         "grouped-df.rkt"
         "grouping.rkt")
(provide (contract-out [left-join (-> (or/c data-frame? grouped-data-frame?)
                                      (or/c data-frame? grouped-data-frame?)
                                      string? ...
                                      (or/c data-frame? grouped-data-frame?))]
                       [right-join (-> (or/c data-frame? grouped-data-frame?)
                                       (or/c data-frame? grouped-data-frame?)
                                       string? ...
                                       (or/c data-frame? grouped-data-frame?))]
                       [inner-join (-> (or/c data-frame? grouped-data-frame?)
                                       (or/c data-frame? grouped-data-frame?)
                                       string? ...
                                       (or/c data-frame? grouped-data-frame?))]
                       [full-join (-> (or/c data-frame? grouped-data-frame?)
                                      (or/c data-frame? grouped-data-frame?)
                                      string? ...
                                      (or/c data-frame? grouped-data-frame?))])
         join-matches join-no-matches)

(define (left-join df1 df2 . by)
  (ignore-groups-apply (left-join-dfs _ (ungroup df2) by) df1))
(define (right-join df1 df2 . by)
  (ignore-groups-apply (left-join-dfs (ungroup df2) _ by) df1))
(define (inner-join df1 df2 . by)
  (ignore-groups-apply (inner-join-dfs _ (ungroup df2) by) df1))
(define (full-join df1 df2 . by)
  (ignore-groups-apply (full-join-dfs _ (ungroup df2) by) df1))

; pad any missing data that isn't matched in any column in df2 with #f
(define (join-no-matches df1 df2-series)
  (define df1-int (sub-data-frame-delegate-frame df1))
  (define return-df (make-data-frame))
  (define df1-size (df-row-count/sub df1))

  (for ([name (in-list df2-series)])
    (df-add-series! return-df (make-series name #:data (make-vector df1-size #f))))
  (for ([name (in-list (df-series-names df1-int))])
    (df-add-series! return-df (make-series name #:data (df-select/sub df1 name))))

  return-df)

; combine on all shared matches
(define (join-matches df1 df2 by)
  (define df1-int (sub-data-frame-delegate-frame df1))
  (define df2-int (sub-data-frame-delegate-frame df2))

  (define (permute-data series df2?)
    (for*/vector ([df1-val (in-data-frame/sub df1 (if df2? (first by) series))]
                  [df2-val (in-data-frame/sub df2 (if df2? series (first by)))])
      (if df2? df2-val df1-val)))
  
  (define return-df (make-data-frame))
  (for ([name (in-list (df-series-names df2-int))])
    (df-add-series! return-df (make-series name #:data (permute-data name #t))))
  (for ([name (in-list (df-series-names df1-int))])
    (df-add-series! return-df (make-series name #:data (permute-data name #f))))

  return-df)

(define left-join-dfs
  (generic-join
   #:on-= (λ (df1 df2 by acc) (cons (join-matches df1 df2 by) acc))
   ; if df2 ends, keep adding #f
   #:on-end
   (λ (df1 df2-names acc)
     (cons (join-no-matches df1 df2-names) acc))
   ; if df1 < df2, add #f
   #:on-<
   (λ (df1 df2-names acc)
     (cons (join-no-matches df1 df2-names) acc))
   ; if df1 > df2, do nothing
   #:on->
   (λ (df1-names df2 acc) acc)))

(define inner-join-dfs
  ; only do something if we're equal
  (generic-join
   #:on-= (λ (df1 df2 by acc) (cons (join-matches df1 df2 by) acc))
   #:on-end (λ (df1 df2-names acc) acc)
   #:on-< (λ (df1 df2-names acc) acc)
   #:on-> (λ (df1-names df2 acc) acc)))

(define full-join-dfs
  ; keep adding #f no matter what
  (generic-join
   #:on-= (λ (df1 df2 by acc) (cons (join-matches df1 df2 by) acc))
   #:on-end
   (λ (df1 df2-names acc)
     (cons (join-no-matches df1 df2-names) acc))
   #:on-<
   (λ (df1 df2-names acc)
     (cons (join-no-matches df1 df2-names) acc))
   #:on->
   (λ (df1-names df2 acc)
     (cons (join-no-matches df2 df1-names) acc))))
