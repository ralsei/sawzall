#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/format
         racket/sequence
         racket/vector)
(provide (contract-out [unnest-longer (->* (data-frame? string?)
                                           (#:keys-to (or/c string? #f)
                                            #:values-to (or/c string? #f)
                                            #:remove? boolean?)
                                           data-frame?)]
                       [unnest-wider (->* (data-frame? string?)
                                          (#:index-prefix string?
                                           #:remove? boolean?)
                                          data-frame?)]))

(define (sequence-return-count seq)
  (call-with-values (λ () (sequence-ref seq 0)) (compose length list)))

;; like in-parallel, but:
;; - if a sequence is #f, make it an endless sequence of #f
;; - if a sequence is shorter than the other sequences specified,
;;   it will pad the end of it with #f
(define (in-parallel* . seqs)
  (define lengths
    (for/list ([v (in-list seqs)])
      (if v (sequence-length v) -1)))
  (define max-len (apply max lengths))
  (define to-add
    (for/list ([len (in-list lengths)])
      (- max-len len)))
  (apply in-parallel
         (for/list ([v (in-list seqs)]
                    [add (in-list to-add)])
           (if v
               (in-sequences v (make-vector add #f))
               (in-cycle (in-value #f))))))

;; takes a column of sequences, and converts it into either one column (if it is list-like),
;; or two columns (if dictionary-like), based on its keys and values
(define (unnest-longer df column-name
                       #:keys-to [keys-to/int #f]
                       #:values-to [values-to/int #f]
                       #:remove? [remove? #t])
  ;; default to column-name + "-keys"
  (define keys-to (if (not keys-to/int)
                      (string-append column-name "-keys")
                      keys-to/int))
  ;; default to column-name
  (define values-to (if (not values-to/int)
                        column-name
                        values-to/int))

  (define data (df-select df column-name))

  ;; determine if the sequence is list-like or dictionary-like
  (define n-return
    (for/first ([seq (in-vector data)]
                #:when seq)
      (sequence-return-count seq)))
  (when (not (or (= n-return 1) (= n-return 2)))
    (error 'unnest-longer "sequence is not list-like or dictionary-like"))
  (define has-keys? (= n-return 2))

  ;; due to arity nonsense, we want to be able to treat it like a dict even
  ;; if it's not one.
  ;; if there's no sequence, convert it to key #f, value #f
  ;; if there is a sequence and we're not dict-like already, make its keys
  ;; an infinite sequence of #f (they don't get used anyway)
  (define padded-data
    (if has-keys?
        data
        (for/vector ([seq (in-vector data)])
          (if seq
              (in-parallel (in-cycle (in-value #f)) seq)
              (in-parallel '(#f) '(#f))))))

  ;; get the lengths of each sequence so we know how many times to duplicate
  ;; the old data
  (define lengths
    (for/list ([seq (in-vector data)])
      (if seq (sequence-length seq) 1)))

  ;; everything but the column values-to, so if it's the same as an input column,
  ;; that input column gets overwritten
  (define everything-else (remove values-to (df-series-names df)))
  (define keys-series
    (if (not has-keys?)
        #f
        (make-series
         keys-to
         #:data (for*/vector ([seq (in-vector padded-data)]
                              [(key _) seq])
                  key))))
  (define values-series
    (make-series
     values-to
     #:data (for*/vector ([seq (in-vector padded-data)]
                          [(_ val) seq])
              val)))

  ;; take the lengths, and then pad each of the data to be that length, to match with
  ;; the newly expanded sequences
  (define old-series
    (for/list ([name (in-list everything-else)])
      (make-series name
                   #:data (apply vector-append
                                 (for/list ([val (in-data-frame df name)]
                                            [len (in-list lengths)])
                                   (make-vector len val))))))

  ;; add everything
  (define return-df (make-data-frame))
  (when has-keys?
    (df-add-series! return-df keys-series))
  (df-add-series! return-df values-series)
  (for ([v (in-list old-series)])
    (df-add-series! return-df v))

  (when (and remove? (not (equal? column-name values-to)))
    (df-del-series! return-df column-name))
  return-df)

;; takes a column of sequences, and turns it into multiple columns
(define (unnest-wider df column-name
                      #:index-prefix [index-prefix "idx-"]
                      #:remove? [remove? #t])
  ;; determine if we are list-like or dictionary-like
  (define data (df-select df column-name))
  (define n-return
    (for/first ([seq (in-vector data)]
                #:when seq)
      (sequence-return-count seq)))
  (when (not (or (= n-return 1) (= n-return 2)))
    (error 'unnest-wider "sequence is not list-like or dictionary-like"))
  (define has-keys? (= n-return 2))

  ;; if we are list-like, then just return the sequence
  ;; if we are dict-like, then get all the values
  (define (get-values sequence)
    (if has-keys?
        (for/vector ([(_ v) sequence])
          v)
        sequence))

  ;; get the maximum length, so we know how many column names to generate
  ;; if we don't have keys
  (define len
    (for/fold ([cur-len 0])
              ([v (in-vector data)])
      (if v
          (max cur-len (sequence-length v))
          cur-len)))
  ;; if we don't have keys, make columns "idx-1", "idx-2", ...
  (define new-column-names
    (if has-keys?
        (for*/vector ([seq (in-vector data)]
                      [(k _) seq])
          k)
        (build-vector len (λ (x) (string-append index-prefix (number->string (add1 x)))))))
  ;; get all values in parallel
  (define parallel-data
    (in-values-sequence (apply in-parallel* (map get-values (vector->list data)))))

  ;; df-shallow-copy because the internals do not change
  (define return-df (df-shallow-copy df))
  ;; for each sequence in the data and each column name, add that column
  (for ([name (in-vector new-column-names)]
        [data parallel-data])
    (df-add-series! return-df (make-series (~a name) #:data (apply vector data))))
  (when remove?
    (df-del-series! return-df column-name))

  return-df)
