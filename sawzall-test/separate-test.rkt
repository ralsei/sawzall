#lang racket/base
(require rackunit
         sawzall
         "util.rkt")

(define has-no-na
  (column-df [col #("a-b" "a-d" "b-c" "d-e")]))
(define has-some-na
  (column-df [col #(#f "a-b" "a-d" "b-c" "d-e")]))

(define extract-1 (extract has-no-na "col" #:into '("A")))
(define extract-1-result
  (column-df [A #("a" "a" "b" "d")]))

(define extract-2 (extract has-no-na "col"
                           #:into '("A" "B")
                           #:regex #px"([[:alnum:]]+)-([[:alnum:]]+)"))
(define extract-2-result
  (column-df [A #("a" "a" "b" "d")]
             [B #("b" "d" "c" "e")]))

;; NA values remain that way
(define extract-3 (extract has-some-na "col" #:into '("A")))
(define extract-3-result
  (column-df [A #(#f "a" "a" "b" "d")]))

(define extract-4 (extract has-some-na "col"
                           #:into '("A" "B")
                           #:regex #px"([[:alnum:]]+)-([[:alnum:]]+)"))
(define extract-4-result
  (column-df [A #(#f "a" "a" "b" "d")]
             [B #(#f "b" "d" "c" "e")]))

;; non-matches become NA
(define extract-5 (extract has-some-na "col"
                           #:into '("A" "B")
                           #:regex #px"([a-d]+)-([a-d]+)"))
(define extract-5-result
  (column-df [A #(#f "a" "a" "b" #f)]
             [B #(#f "b" "d" "c" #f)]))

;; skip variables with #f
(define extract-6 (extract has-some-na "col"
                           #:into '("A" #f)
                           #:regex #px"([a-d]+)-([a-d]+)"))
(define extract-6-result
  (column-df [A #(#f "a" "a" "b" #f)]))

;; don't remove original column
(define extract-7 (extract has-some-na "col"
                           #:into '("A" #f)
                           #:regex #px"([a-d]+)-([a-d]+)"
                           #:remove? #f))
(define extract-7-result
  (column-df [col #(#f "a-b" "a-d" "b-c" "d-e")]
             [A #(#f "a" "a" "b" #f)]))

(module+ test
  (check data-frame~=? extract-1 extract-1-result)
  (check data-frame~=? extract-2 extract-2-result)
  (check data-frame~=? extract-3 extract-3-result)
  (check data-frame~=? extract-4 extract-4-result)
  (check data-frame~=? extract-5 extract-5-result)
  (check data-frame~=? extract-6 extract-6-result)
  (check data-frame~=? extract-7 extract-7-result))
