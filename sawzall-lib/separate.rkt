#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/match
         racket/string
         racket/vector
         "helpers.rkt")
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
                                     data-frame?)]
                       [unite (->* (data-frame?
                                    string?
                                    #:from (non-empty-listof string?))
                                   (#:combine (-> any/c ... any/c)
                                    #:remove? boolean?)
                                   data-frame?)]))

;; creates substrings from a given list of indices
;; example:
;;   (substrings "asdfghjkl" (list 1 3 5 7))
;;   => (vector "a" "sd" "fg" "hj" "kl")
(define (substrings s offset-list)
  (define len (string-length s))
  (for/vector ([start (in-sequences (list 0) offset-list)]
               [end   (in-sequences offset-list (list len))])
    (substring s start end)))

;; turns a column of strings into multiple columns of strings,
;; on some separator
(define (separate df column-name
                  #:into new-column-names
                  #:separator [separator #px"[^[:alnum:]]+"]
                  #:remove? [remove? #t]
                  #:fill [fill 'right])
  (define (split str)
    (cond [(not str) #f]
          [(or (regexp? separator) (string? separator))
           (apply vector (regexp-split separator str))]
          [(number? separator) (substrings str (list separator))]
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
  (define padded (vector-map (? pad-or-truncate-vector _ max-len) split-up))

  (define return-df (df-shallow-copy df))
  (for ([(name idx) (in-indexed (in-list new-column-names))]
        #:when name)
    (df-add-series! return-df (make-series name #:data (vector-map (? vector-ref _ idx) padded))))
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
    (define res (? cdr (regexp-match regex str)))
    (cond [res
           (when (not (= (length res) new-column-len))
             (error 'extract "too many/too few matches for input ~a under pattern ~a" str regex))
           (apply vector res)]
          [else #f]))

  (define data (df-select df column-name))
  (define split-up (vector-map (λ? capture) data))

  (define return-df (df-shallow-copy df))
  (for ([(name idx) (in-indexed (in-list new-column-names))]
        #:when name)
    (df-add-series! return-df (make-series name #:data (vector-map (? vector-ref _ idx) split-up))))
  (when remove?
    (df-del-series! return-df column-name))
  return-df)

;; turns multiple columns as specified by `parsed-spec` into one column,
;; by applying `combine-fn`
(define (unite df column-name
               #:from to-combine
               #:combine [combine-fn (λ args (string-join (filter (λ (x) x) args) "_"))]
               #:remove? [remove? #t])
  (define new-series
    (make-series column-name
                 #:data (for/vector ([data (apply in-data-frame/list df to-combine)])
                          (apply combine-fn data))))

  (define return-df (df-shallow-copy df))
  (df-add-series! return-df new-series)
  (when remove?
    (for ([v (in-list to-combine)])
      (df-del-series! return-df v)))
  return-df)
