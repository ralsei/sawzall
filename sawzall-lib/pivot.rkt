#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/list
         racket/set
         racket/vector)
(provide (contract-out [pivot-longer (-> data-frame? (non-empty-listof string?)
                                         #:names-to string? #:values-to string?
                                         data-frame?)]))

; lengthens data, increasing the number of rows and decreasing the number of columns
(define (pivot-longer df cols
                      #:names-to name
                      #:values-to value)
  (define return-df (make-data-frame))

  (define ~cols (set-subtract (df-series-names df) cols))
  (define n-new-cols (length cols))
  ; each column becomes a set of rows = to the length of the df
  ; so replicate retained series based on the number of columns
  (define new-~col-series
    (for/list ([name (in-list ~cols)])
      (make-series name
                   #:data (apply vector-append
                                 (make-list n-new-cols (df-select df name))))))

  ; turn each series name into a new series that's comprised of its names repeated by the number
  ; of rows
  (define n-rows (df-row-count df))
  (define new-name-series
    (make-series name #:data (apply vector-append (map (make-vector n-rows _) cols))))
  ; and append all the values into their own series, which match with the names by virtue of ordering
  (define new-val-series
    (make-series value #:data (apply vector-append (map (df-select df _) cols))))

  (for ([s (in-list new-~col-series)])
    (df-add-series! return-df s))
  (df-add-series! return-df new-name-series)
  (df-add-series! return-df new-val-series)

  return-df)

(define test-df (make-data-frame))

(df-add-series! test-df (make-series "day" #:data (vector 1 1 2 2)))
(df-add-series! test-df (make-series "hour" #:data (vector 10 11 10 11)))
(df-add-series! test-df (make-series "a" #:data (vector 97 78 83 30)))
(df-add-series! test-df (make-series "b" #:data (vector 84 47 73 46)))
(df-add-series! test-df (make-series "c" #:data (vector 55 54 38 58)))
