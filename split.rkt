#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/set
         racket/vector
         "helpers.rkt")
(provide
 (contract-out [split-with (-> data-frame? string? (listof data-frame?))]
               [combine (->* () #:rest (non-empty-listof data-frame?) data-frame?)]))

; defines the split operation, which constructs multiple data-frames from
; an existing data-frame.
; does NOT produce another data-frame, instead producing a list of data-frames
; split into unique groups.
(define (split-with df group)
  (define (df-with possibility)
    (define return-df (make-data-frame))
    ; TODO: data-frame uses bsearch for this
    (define possibility-indices
      (for/list ([(v idx) (in-indexed (in-vector (df-select df group)))]
                 #:when (equal? v possibility))
        idx))
    (define new-series
      (for/list ([col (in-list (df-series-names df))])
        (make-series col #:data (for/vector ([idx (in-list possibility-indices)])
                                  (df-ref df idx col)))))
    (for ([s (in-list new-series)])
      (df-add-series! return-df s))
    return-df)
  (vector->list (vector-map df-with (possibilities df group))))

; shared series between data-frames
(define (shared-series dfs)
  (apply set-intersect (map (compose list->set df-series-names) dfs)))

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
             (make-series col #:data (for/fold ([v (vector)])
                                               ([df (in-list dfs)])
                                       (vector-append v (df-select df col))))))
         (for ([s (in-list new-series)])
           (df-add-series! return-df s))
         return-df]))
