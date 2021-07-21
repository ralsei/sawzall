#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/list
         racket/match
         "bsearch.rkt"
         "grouped-df.rkt"
         "helpers.rkt"
         "reorder-df.rkt"
         "split.rkt")
(provide
 (contract-out [group-with (->* (data-frame?)
                                #:rest (non-empty-listof string?)
                                grouped-data-frame?)]
               [ungroup-once (-> (or/c data-frame? grouped-data-frame?)
                                 (or/c data-frame? grouped-data-frame?))]
               [ungroup (-> (or/c data-frame? grouped-data-frame?)
                            data-frame?)])
 grouped-df-apply ignore-groups-apply)

;;;; constructing grouped data frames
(define (group-with df . groups)
  (define sorted (reorder-default df groups))

  (define (build-group-vector grp [in-ivls (vector (ivl 0 (df-row-count df)))])
    (define vec (df-select sorted grp))
    (for*/vector ([interval (in-vector in-ivls)]
                  [p (in-vector (possibilities sorted grp #:ivl interval))])
      (match-define (ivl beg end) interval)
      (call-with-values
       (λ () (equal-range vec p #:cmp orderable<? #:start beg #:stop end))
       ivl)))

  (define grp-vecs
    (for/fold ([vecs '()])
              ([grp (in-list groups)])
      (if (null? vecs)
          (cons (build-group-vector grp) vecs)
          (cons (build-group-vector grp (first vecs)) vecs))))

  (grouped-data-frame sorted (reverse groups) grp-vecs))

(define (ungroup-once gdf)
  (cond [(data-frame? gdf) gdf]
        [else
         (match-define (grouped-data-frame df grps grp-idxes) gdf)
         (if (empty? (rest grps))
             df
             (grouped-data-frame df (rest grps) (rest grp-idxes)))]))

(define (ungroup gdf)
  (if (grouped-data-frame? gdf)
      (grouped-data-frame-delegate-frame gdf)
      gdf))

;; applies a function (sub-data-frame? -> data-frame?) to a grouped data frame
(define (grouped-df-apply fn df #:pass-groups? [pass-groups? #f])
  (cond [(data-frame? df) (if pass-groups? (fn (df-with-ivl df) null) (fn (df-with-ivl df)))]
        [(sub-data-frame? df) (if pass-groups? (fn df null) (fn df))]
        [(grouped-data-frame? df)
         (match-define (grouped-data-frame int-df grps grp-idxes) df)
         (define (call ivl)
           (if pass-groups?
               (fn (df-with-ivl int-df ivl) grps)
               (fn (df-with-ivl int-df ivl))))
         (apply group-with
                (apply combine
                       (for/list ([i (in-vector (first grp-idxes))])
                         (call i)))
                (reverse grps))]))

;; applies a function (data-frame? -> data-frame?) to a grouped data frame,
;; ignoring its grouping
(define (ignore-groups-apply fn df #:pass-groups? [pass-groups? #f] #:regroup? [regroup? #t])
  (define real-df
    (cond [(grouped-data-frame? df) (grouped-data-frame-delegate-frame df)]
          [(sub-data-frame? df) (sub-data-frame-delegate-frame df)]
          [(data-frame? df) df]))
  (define groups
    (cond [(grouped-data-frame? df) (grouped-data-frame-groups df)]
          [else null]))
  (define res (if pass-groups? (fn real-df groups) (fn real-df)))
  ((if (and regroup? (grouped-data-frame? df))
       (apply group-with _ (reverse groups))
       (λ (x) x))
   res))
