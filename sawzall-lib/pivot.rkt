#lang racket/base
(require (for-syntax racket/base)
         data-frame
         fancy-app
         racket/contract/base
         racket/format
         racket/list
         racket/set
         racket/vector
         syntax/parse/define
         "combining-join.rkt"
         "rename.rkt"
         "slice-spec.rkt"
         "split.rkt")
(provide pivot-longer
         (contract-out [pivot-wider (-> data-frame?
                                        #:names-from string? #:values-from string?
                                        data-frame?)]))

(define-syntax-parse-rule (pivot-longer df spec:slice-spec
                                        #:names-to name #:values-to value)
  #:declare df (expr/c #'data-frame?)
  #:declare name (expr/c #'string?)
  #:declare value (expr/c #'string?)
  (pivot-longer/int df 'spec #:names-to name #:values-to value))

; lengthens data, increasing the number of rows and decreasing the number of columns
(define (pivot-longer/int df quoted-spec
                          #:names-to name
                          #:values-to value)
  (define cols (exec-spec-on-df df quoted-spec))

  (define ~cols (set-subtract (apply set (df-series-names df)) cols))
  (define n-new-cols (set-count cols))
  ; each column becomes a set of rows = to the length of the df
  ; so replicate retained series based on the number of columns
  (define new-~col-series
    (for/list ([name (in-set ~cols)])
      (make-series name
                   #:data (apply vector-append
                                 (make-list n-new-cols (df-select df name))))))

  ; turn each series name into a new series that's comprised of its names repeated by the number
  ; of rows
  (define n-rows (df-row-count df))
  (define list-cols (set->list cols))
  (define new-name-series
    (make-series name #:data (apply vector-append (map (make-vector n-rows _) list-cols))))
  ; and append all the values into their own series, which match with the names by virtue of ordering
  (define new-val-series
    (make-series value #:data (apply vector-append (map (df-select df _) list-cols))))

  (define return-df (make-data-frame))
  (for ([s (in-list new-~col-series)])
    (df-add-series! return-df s))
  (df-add-series! return-df new-name-series)
  (df-add-series! return-df new-val-series)

  return-df)

; widens data, decreasing the number of rows and increasing the number of columns
; XXX: this is not particularly efficient, but intuitive. should see what dplyr does
(define (pivot-wider df #:names-from name-from #:values-from value-from)
  (define split (split-with-possibility df name-from))

  (define to-ljoin
    (for/list ([on-possibility (in-vector split)])
      (define val (car on-possibility))
      (define int-df (cdr on-possibility))

      (define ret (rename int-df value-from (~a val)))
      (df-del-series! ret name-from)
      ret))

  (define return-df
    (for/fold ([d (first to-ljoin)])
              ([v (in-list (rest to-ljoin))])
      (left-join v d)))

  return-df)
