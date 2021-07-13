#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/set
         racket/vector
         "helpers.rkt"
         "reorder-df.rkt")
(provide
 (contract-out [split-with (-> data-frame? string? (listof data-frame?))]
               [combine (->* () #:rest (non-empty-listof data-frame?) data-frame?)])
 split-with-possibility)

; splitting, and recording the possibility
(define (split-with-possibility df group)
  (define sorted-df (reorder-df df (list (cons group orderable<?))))

  (define (make-indexer possibility)
    (define not-really-sorted? (not (df-is-sorted? sorted-df group)))
    (define-values (beg end) (series-equal-range (df-duplicate-series sorted-df group) possibility))
    (displayln beg)
    (displayln end)
    (define indices
      (if not-really-sorted?
          (for/list ([(v idx) (in-indexed (in-data-frame sorted-df group))]
                     #:when (equal? v possibility))
            idx)
          (call-with-values (λ () (df-equal-range sorted-df group possibility))
                            cons)))
    (λ (column-name)
      (make-series column-name
                   #:data
                   (if not-really-sorted?
                       (for/vector ([idx (in-list indices)]) (df-ref sorted-df idx column-name))
                       (vector-copy (df-select sorted-df column-name)
                                    (car indices) (cdr indices))))))

  (define (df-with possibility)
    (define return-df (make-data-frame))
    (define indexer (make-indexer possibility))
    (define new-series (map indexer (df-series-names sorted-df)))
    (for ([s (in-list new-series)])
      (df-add-series! return-df s))
    (cons possibility return-df))

  (vector-map df-with (possibilities sorted-df group)))

; defines the split operation, which constructs multiple data-frames from
; an existing data-frame.
; does NOT produce another data-frame, instead producing a list of data-frames
; split into unique groups.
(define (split-with df group)
  (for/list ([v (in-vector (split-with-possibility df group))])
    (cdr v)))

; shared series between data-frames
(define (shared-series dfs)
  (apply set-intersect (map df-series-names dfs)))

; binding a faceted list of data-frames into a singular data-frame
; assumption: the data-frames have at least one common column
(define (combine . dfs)
  (cond [(= (length dfs) 1) (car dfs)]
        [else
         (define series-names (shared-series dfs))
         (when (set-empty? series-names)
           (error 'combine "no common columns in data-frames"))
         (define return-df (make-data-frame))
         (define new-series
           (for/list ([col (in-set series-names)])
             (make-series col #:data (apply vector-append (map (df-select _ col) dfs)))))
         (for ([s (in-list new-series)])
           (df-add-series! return-df s))
         return-df]))
