#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         text-table
         racket/contract/base
         racket/format
         racket/list
         syntax/parse/define
         "grouped-df.rkt"
         "grouping.rkt"
         "slice-spec.rkt")
(provide (contract-out [sawzall-show-formatter (parameter/c (-> any/c string?))])
         show introspect)

(define sawzall-show-formatter (make-parameter ~a))

(define *show-rows-default* 6)
(define *show-cols-default* 6)

(define (take* lst n)
  (cond [(zero? n) lst]
        [(empty? lst) lst]
        [else (cons (first lst)
                    (take* (rest lst) (sub1 n)))]))

(define (gdf-series-names df)
  (df-series-names
   (cond [(grouped-data-frame? df) (grouped-data-frame-delegate-frame df)]
         [(sub-data-frame? df) (sub-data-frame-delegate-frame df)]
         [(data-frame? df) df])))

(define-for-syntax (show-syntax-form stx return?)
  (syntax-parse stx
    [(_ df {~alt
            {~optional spec:slice-spec
                       #:defaults ([spec.parsed #'(all-in$
                                                   (take*
                                                    (gdf-series-names df)
                                                    *show-cols-default*))])}
            {~optional {~seq #:n-rows n-rows}
                       #:defaults ([n-rows.c #'*show-rows-default*])}}
        ...)
     #:declare df (expr/c #'(or/c data-frame? grouped-data-frame?))
     #:declare n-rows (expr/c #'(or/c exact-nonnegative-integer? 'all))
     #`(begin
         (ignore-groups-apply (show-internal spec.parsed n-rows.c) df.c
                              #:pass-groups? #t #:regroup? #f)
         #,(if return? #'df #'(void)))]))

(define-syntax (show stx)
  (show-syntax-form stx #f))
(define-syntax (introspect stx)
  (show-syntax-form stx #t))

(define ((show-internal parsed-spec row-cap/i) df grps)
  (define all-series (exec-spec-on-df df parsed-spec))

  (define all? (eq? row-cap/i 'all))
  (define n-rows (df-row-count df))
  (define n-cols (length (df-series-names df)))
  (define row-cap (and (not all?) (min n-rows row-cap/i)))
  (define col-cap (length all-series))

  (printf "data-frame: ~a rows x ~a columns~n" n-rows n-cols)
  (when (not (null? grps))
    (printf "groups: ~a~n" grps))

  (displayln
   (table->string
    #:->string (sawzall-show-formatter)
    (let ([series (if all? all-series (take all-series col-cap))])
      (cons series
            (for/list ([v (apply in-data-frame/list df series)]
                       [_ (if all? n-rows row-cap)])
              v)))))

  (when (not (or all? (and (= n-rows row-cap) (= n-cols col-cap))))
    (printf "~a rows, ~a cols elided (use #:all? for full frame)~n"
            (- n-rows row-cap)
            (- n-cols col-cap))))
