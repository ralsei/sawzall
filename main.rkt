#lang racket
(require (for-syntax racket/base)
         data-frame
         syntax/parse/define)

(struct saw-proc (columns bindings procs))

(define-syntax (saw-λ stx)
  (syntax-parse stx
    [(_ [col binding body ...] ...)
     #'(saw-proc (list (symbol->string (quote col)) ...)
                 (list (symbol->string (quote binding)) ...)
                 (list
                  (λ binding ...
                    body ...)
                  ...))]))
; entry-point
(define gss-sm (df-read/csv "data/gss_sm.csv"))
; group by bigregion and religion, make a new column total that is
; the total of each religion in each region, _remove other columns_
;; (define aggregated
;;   (aggregate gss-sm
;;              '(bigregion religion)
;;              (saw-λ [total (religion) (sum religion)]))) ; argument is a vector here
;; ; count the number of total observations (maybe needs a better abstraction?)
;; (define n-observations
;;   (for/sum ([v (in-data-frame aggregated "total")])
;;     v))
;; ; make a new column that is the frequency of each religion's occurrence,
;; ; then convert it to pct%
;; (define with-frequency
;;   (create aggregated
;;           (saw-λ [frequency (total) (/ total n-observations)] ; argument is a value here, mapped
;;                  [percent (frequency) (round (* frequency 100))])))
