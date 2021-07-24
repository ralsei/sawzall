#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/function
         racket/match
         racket/vector)
(provide (contract-out [separate (->* (data-frame?
                                       string?
                                       #:into (non-empty-listof (or/c string? #f)))
                                      (#:separator (or/c string?
                                                         regexp?
                                                         exact-nonnegative-integer?
                                                         (listof
                                                          exact-nonnegative-integer?))
                                       #:remove? boolean?
                                       #:fill (or/c 'right 'left))
                                      data-frame?)]
                       [extract (->* (data-frame?
                                      string?
                                      #:into (non-empty-listof (or/c string? #f)))
                                     (#:regex regexp?
                                      #:remove? boolean?)
                                     data-frame?)]))

;; splits a string at a given index
(define (string-split-at str n)
  (values (substring str 0 n)
          (substring str n)))

;; creates substrings from a given list of indices
;; example:
;;   (substrings "asdfghjkl" (list 1 3 5 7))
;;   => (list "a" "sd" "fg" "hj" "kl")
(define (substrings str lst)
  (apply
   vector
   (let loop ([substr str] [offset 0] [idxes lst])
     (match idxes
       ['() (list substr)]
       [`(,idx . ,rst)
        (define-values (l r) (string-split-at substr (- idx offset)))
        (cons l (loop r idx rst))]))))

;; turns a column of strings into multiple columns of strings,
;; on some separator
(define (separate df column-name
                  #:into new-column-names
                  #:separator [separator #px"[^[:alnum:]]+"]
                  #:remove? [remove? #t]
                  #:fill [fill 'right])
  (define (split str)
    (cond [(or (regexp? separator) (string? separator))
           (apply vector (regexp-split separator str))]
          [(number? separator)
           (call-with-values (thunk (string-split-at str separator)) vector)]
          [(list? separator) (substrings str separator)]
          [else #f]))
  ;; what to do with matches that don't match the rest
  (define (pad-or-truncate-vector vec target-len)
    (define input-len (vector-length vec))
    (cond [(> target-len input-len)
           (define return-vec (make-vector target-len #f))
           (define beg-idx
             (match fill
               ['left  (- target-len input-len)]
               ['right 0]))
           (vector-copy! return-vec beg-idx vec)
           return-vec]
          [(< target-len input-len)
           (define return-vec (make-vector target-len #f))
           (vector-copy! return-vec 0 vec target-len)
           return-vec]
          [else vec]))

  (define data (df-select df column-name))
  (define split-up (vector-map split data))
  (define max-len
    (for/fold ([cur-max 0])             ; we are only dealing with positive exact integers
              ([v (in-vector split-up)]
               #:when v)
      (max cur-max (vector-length v))))
  (define padded (vector-map (pad-or-truncate-vector _ max-len) split-up))

  (define return-df (df-shallow-copy df))
  (for ([(name idx) (in-indexed (in-list new-column-names))]
        #:when name)
    (df-add-series! return-df (make-series name #:data (vector-map (vector-ref _ idx) padded))))
  (when remove?
    (df-del-series! return-df column-name))
  return-df)

;; turns a column of strings into multiple columns of strings,
;; by using regular expression capturing groups
(define (extract df column-name
                 #:into new-column-names
                 #:regex [regex #px"([[:alnum:]]+)"]
                 #:remove? [remove? #t])
  (define new-column-len (length new-column-names))
  (define (capture str)
    (cond [str
           (define res (regexp-match regex str))
           (cond [res
                  (when (not (> (length res) new-column-len))
                    (error 'extract "too many matches for input ~a under pattern ~a" str regex))
                  (apply vector (cdr res))]
                 [else #f])]
          [else #f]))
  (define (vector-ref/f vec idx)
    (cond [vec (vector-ref vec idx)]
          [else #f]))

  (define data (df-select df column-name))
  (define split-up (vector-map capture data))

  (define return-df (df-shallow-copy df))
  (for ([(name idx) (in-indexed (in-list new-column-names))]
        #:when name)
    (df-add-series! return-df (make-series name #:data (vector-map (vector-ref/f _ idx) split-up))))
  (when remove?
    (df-del-series! return-df column-name))
  return-df)
