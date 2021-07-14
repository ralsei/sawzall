#lang racket/base
(require data-frame
         racket/function
         racket/list
         racket/match
         racket/vector
         "helpers.rkt")
(provide (struct-out sub-data-frame)
         (struct-out ivl)
         df-select/sub
         in-data-frame/sub
         in-data-frame/list/sub
         df-with-ivl)

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
  (apply (curry in-data-frame df #:start beg #:stop end) series))

(define (in-data-frame/list/sub dfl . series)
  (match-define (sub-data-frame df (ivl beg end)) dfl)
  (apply (curry in-data-frame/list df #:start beg #:stop end) series))

;; add an interval to a data-frame, or alternatively add an interval that comprises
;; the entire df
(define (df-with-ivl df [int #f])
  (sub-data-frame df (if int int (ivl 0 (df-row-count df)))))

;;;; sorting by groups
;; sorts the given data-frame by the given list of groups.
;; unlike `reorder`, this behaves slightly differently, sorting by `grp` first
;; and only using `grp2` to break ties, etc for others.
;;
;; TODO: this should support something other than `orderable<?`. how do we want
;; to specify that?
(define (sort-with-groups df grps)
  ;; takes two indices, determines which is <? the other w.r.t the groups
  (define (cmp-fn a-idx b-idx)
    (define a-vals (apply df-ref* df a-idx grps))
    (define b-vals (apply df-ref* df b-idx grps))
    (for/or ([a (in-vector a-vals)]
             [b (in-vector b-vals)])
      (orderable<? a b)))

  (define indices (vector-sort (build-vector (df-row-count df) (λ (x) x)) cmp-fn))
  (define return-df (make-data-frame))
  (for ([s (in-list (df-series-names df))])
    (df-add-series! return-df (make-series s #:data (vector-reorder (df-select df s) indices))))
  return-df)

;;;; grouped data frames
;; a data-frame, with some additional information:
;; - the groups
;; - for each group, a set of intervals that correspond to an ivl (for use with
;;   a `sub-data-frame`)
;;
;; unlike a regular data-frame, this is immutable to avoid destroying invariants
(struct grouped-data-frame (delegate-frame groups group-indices) #:transparent)

;; applies a function (sub-data-frame? -> data-frame?) to a grouped data frame
(define (grouped-df-apply fn df #:pass-groups? [pass-groups? #f])
  3)

;; applies a function (sub-data-frame? -> data-frame?) to a grouped data frame,
;; ignoring its grouping
(define (ignore-groups-apply fn df #:pass-groups? [pass-groups? #f])
  3)

(require "constructors.rkt")
(define test-df
  (row-df [grp grp2 val]
          "A"  "X"  1
          "A"  "X"  2
          "A"  "Y"  3
          "B"  "F"  4
          "B"  "G"  5
          "B"  "H"  6))
(define test-df2
  (row-df [grp grp2 val]
          "B" "H" 6
          "A" "Y" 3
          "A" "X" 2
          "B" "G" 5
          "A" "X" 1
          "B" "F" 4))

;; test-df1 should be ~= test-df2 (not equal? for various reasons)
