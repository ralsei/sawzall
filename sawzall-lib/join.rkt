#lang racket/base
(require data-frame
         fancy-app
         "helpers.rkt"
         "grouping.rkt"
         "reorder.rkt"
         "syntax.rkt")

(define (left-join df1 df2 by #:cmp? [cmp? orderable<?])
  (define sorted-df1 (reorder/int df1 (sort-proc (list by) (list cmp?))))
  (define sorted-df2 (reorder/int df2 (sort-proc (list by) (list cmp?))))
  9)
