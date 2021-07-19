#lang racket/base
(require data-frame
         racket/contract/base
         racket/match)
(provide (struct-out ivl)
         df-select/sub
         in-data-frame/sub
         in-data-frame/list/sub
         df-dumb-copy/sub
         sub-df-empty?
         df-ref/sub
         df-with-ivl

         (contract-out
          [struct sub-data-frame ((delegate-frame data-frame?)
                                  (ivl ivl?))]
          [struct grouped-data-frame ((delegate-frame data-frame?)
                                      (groups (listof string?))
                                      (group-indices (listof (vectorof ivl?))))]))

;;;; subframes
;; like a data-frame, except only a contiguous slice of it
(struct ivl (beg end) #:transparent)
(struct sub-data-frame (delegate-frame ivl) #:transparent)

;; like their regular data-frame functions, but operating on sub-data-frames
(define (df-select/sub dfl series)
  (match-define (sub-data-frame df (ivl beg end)) dfl)
  (df-select df series #:start beg #:stop end))

(define (in-data-frame/sub dfl . series)
  (match-define (sub-data-frame df (ivl beg end)) dfl)
  (apply in-data-frame df #:start beg #:stop end series))

(define (in-data-frame/list/sub dfl . series)
  (match-define (sub-data-frame df (ivl beg end)) dfl)
  (apply in-data-frame/list df #:start beg #:stop end series))

(define (df-ref/sub dfl idx series)
  (match-define (sub-data-frame df (ivl beg end)) dfl)
  (df-ref df (+ beg idx) series))

(define (df-dumb-copy/sub dfl)
  (match-define (sub-data-frame df (ivl beg end)) dfl)
  (define return-df (make-data-frame))
  (for ([s (in-list (df-series-names df))])
    (df-add-series! return-df
                    (make-series s #:data (df-select df s #:start beg #:stop end))))
  return-df)

(define (sub-df-empty? dfl)
  (match-define (sub-data-frame _ (ivl beg end)) dfl)
  (= (- end beg) 0))

;; add an interval to a data-frame, or alternatively add an interval that comprises
;; the entire df
(define (df-with-ivl df [int #f])
  (sub-data-frame df (if int int (ivl 0 (df-row-count df)))))

;;;; grouped data frames
;; a data-frame, with some additional information:
;; - the groups
;; - for each group, a set of intervals that correspond to an ivl (for use with
;;   a `sub-data-frame`)
;;
;; unlike a regular data-frame, this is immutable to avoid destroying invariants
(struct grouped-data-frame (delegate-frame groups group-indices) #:transparent)
