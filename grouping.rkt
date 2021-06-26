#lang racket/base
(require data-frame
         fancy-app
         racket/contract/base
         racket/list
         racket/set
         racket/vector
         "helpers.rkt")
(provide
 (contract-out [struct grouped-data-frame ((groups (non-empty-listof string?))
                                           (frames (non-empty-listof data-frame?)))]
               [group-with (->* ((or/c data-frame? grouped-data-frame?))
                                #:rest (non-empty-listof string?)
                                grouped-data-frame?)]
               [ungroup (-> (or/c data-frame? grouped-data-frame?)
                            (or/c data-frame? grouped-data-frame?))]
               [get-groups (-> (or/c grouped-data-frame? data-frame?) (listof string?))]
               [get-frames (-> (or/c grouped-data-frame? data-frame?)
                               (non-empty-listof data-frame?))])
 return-with-list)

(struct grouped-data-frame (groups frames) #:transparent)

; defines the `group` operation, which constructs multiple data-frames from
; an existing data-frame.
; does NOT produce another data-frame, instead producing a structure encoding
; the data-frames and its groups
(define (group-once df group)
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

; the above, but it can do multiple groups
(define (group-with df . groups)
  (grouped-data-frame
   groups
   (for/fold ([dfs (list df)])
             ([grp (in-list groups)])
     (map (group-once _ grp) dfs))))

; shared series between data-frames
(define (shared-series dfs)
  (apply set-intersect (map (compose list->set df-series-names) dfs)))

; binding a faceted list of data-frames into a singular data-frame
; assumption: the data-frames have at least one common column
(define (ungroup grouped-df)
  (define dfs (grouped-data-frame-frames grouped-df))
  (cond [(= (length dfs) 1) (car dfs)]
        [else
         (define series-names (shared-series dfs))
         (when (set-empty? series-names)
           (error 'ungroup "no common columns in data-frames. this is really bad, report a bug"))
         (define return-df (make-data-frame))
         (define new-series
           (for/list ([col (in-set series-names)])
             (make-series col #:data (for/fold ([v (vector)])
                                               ([df (in-list dfs)])
                                       (vector-append v (df-select df col))))))
         (for ([s (in-list new-series)])
           (df-add-series! return-df s))
         return-df]))

(define (get-groups df)
  (cond [(grouped-data-frame? df) (grouped-data-frame-groups df)]
        [else '()]))

(define (get-frames df)
  (cond [(grouped-data-frame? df) (grouped-data-frame-frames df)]
        [else (list df)]))

(define (return-with-list df lst #:peel? [peel? #f])
  (if (= (length lst) 1)
      (first lst)
      (grouped-data-frame (if peel? (rest (get-groups df)) (get-groups df))
                          (if peel?
                              (ungroup (grouped-data-frame (get-groups df) lst))
                              lst))))
