#lang racket/base
(require data-frame
         racket/contract/base)
(provide (contract-out [column-df (-> (listof string?) (listof (listof any/c)) data-frame?)]
                       [row-df (-> (listof string?) (listof (listof any/c)) data-frame?)]))

(define (column-df column-names column-list)
  (when (not (= (length column-names) (length column-list)))
    (error 'column-df "column-names and column-list must be same length"))

  (define return-df (make-data-frame))

  (for ([name (in-list column-names)]
        [col (in-list column-list)])
    (df-add-series! return-df (make-series name #:data (list->vector col))))

  return-df)

(define (row-df column-names row-list)
  (column-df column-names (apply map list row-list)))
