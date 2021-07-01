#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/list
         racket/match
         "split.rkt")
(provide
 (contract-out [struct grouped-data-frame ((groups (listof string?))
                                           (frames (non-empty-listof (or/c data-frame?
                                                                           grouped-data-frame?))))]
               [group-with (->* ((or/c data-frame? grouped-data-frame?))
                                #:rest (non-empty-listof string?)
                                grouped-data-frame?)]
               [ungroup (-> (or/c data-frame? grouped-data-frame?)
                            (or/c data-frame? grouped-data-frame?))]
               [ungroup-all (-> (or/c data-frame? grouped-data-frame?)
                                (or/c data-frame? grouped-data-frame?))])
 listof-data-frames? ->grouped-data-frame
 group-map ignore-grouping)

(struct grouped-data-frame (groups frames) #:transparent)

(define (->grouped-data-frame df)
  (cond [(grouped-data-frame? df) df]
        [else (grouped-data-frame null (list df))]))

(define (group-map appl df #:pass-groups? [pass-groups? #f])
  (define (iter d acc)
    (cond [(grouped-data-frame? d)
           (match d
             [(grouped-data-frame grps dfs)
              (grouped-data-frame grps (map (iter _ grps) dfs))])]
          [(data-frame? d)
           (if pass-groups?
               (appl d acc)
               (appl d))]))
  (iter df null))

(define (ignore-grouping appl df #:pass-groups? [pass-groups? #f] #:regroup? [regroup? #t])
  (define (all-groups gdf)
    (cond [(data-frame? gdf) (list)]
          [(grouped-data-frame? gdf)
           (match-define (grouped-data-frame grps dfs) gdf)
           (if (listof-data-frames? dfs)
               grps
               ; all the same, so just take the first
               (all-groups (first dfs)))]))
  ; strip grouping, apply, then regroup
  (define grps (all-groups df))
  (define merged-df (ungroup-all df))
  (define res (if pass-groups? (appl merged-df grps) (appl merged-df)))
  (if regroup? (apply group-with res grps) res))

(define (group-with df . groups)
  (define (iter d grps acc)
    (match grps
      ['() d]
      [`(,fst . ,rst)
       (define new-acc (cons fst acc))
       (grouped-data-frame
        new-acc
        (map (iter _ rst new-acc) (split-with d fst)))]))
  (iter df groups '()))

(define (ungroup grouped-df)
  (cond [(data-frame? grouped-df) grouped-df]
        [else
         (match-define (grouped-data-frame grp dfs) grouped-df)
         (cond [(listof-data-frames? dfs)
                (apply combine dfs)]
               [else (grouped-data-frame grp (map ungroup dfs))])]))

(define (fix f xs)
  (let ([next (f xs)])
    (if (equal? next xs)
        xs
        (fix f next))))

(define (ungroup-all grouped-df)
  (fix ungroup grouped-df))

; meh, no real need to check all of it -- lists are probably homogenous unless something
; goes horribly wrong
(define (listof-data-frames? lst)
  (and (list? lst) (data-frame? (first lst))))
