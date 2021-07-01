#lang racket/base
(require (for-syntax racket/base
                     syntax/for-body
                     syntax/parse)
         data-frame)
(provide for/data-frame for*/data-frame)

(define (grow-vector vec)
  (define n (vector-length vec))
  (define new-vec (make-vector (* 2 n)))
  (vector-copy! new-vec 0 vec 0 n)
  new-vec)

(define (shrink-vector vec i)
  (define new-vec (make-vector i))
  (vector-copy! new-vec 0 vec 0 i)
  new-vec)

(define (make-df-with-series col-names series)
  (define new-df (make-data-frame))
  (for ([name (in-list col-names)]
        [vec (in-list series)])
    (df-add-series! new-df (make-series name #:data vec)))
  new-df)

(define-for-syntax (for_/data-frame stx for_/fold/derived-stx)
  (syntax-parse stx
    [(_ (column:id ...) clauses body ... tail-expr)
     #:with orig-stx stx
     #:with for_/fold/derived for_/fold/derived-stx
     #:with ((pre-body ...) (post-body ...)) (split-for-body stx #'(body ... tail-expr))
     #'(let ([columns (list (symbol->string 'column) ...)])
         (call-with-values
          (λ ()
            (for_/fold/derived orig-stx
              ([i 0]
               [column (make-vector 16)] ...)
              clauses pre-body ...

              (apply values
                     (add1 i)
                     (for/list ([col-vec (in-list (list column ...))]
                                [value (in-list (call-with-values (λ () post-body ...) list))])
                       (define new-vec (if (eq? i (vector-length col-vec))
                                           (grow-vector col-vec)
                                           col-vec))
                       (vector-set! new-vec i value)
                       new-vec))))
          (λ vecs (make-df-with-series columns
                                       (map (λ (v) (shrink-vector v (car vecs)))
                                            (cdr vecs))))))]))

(define-syntax (for/data-frame stx)
  (for_/data-frame stx #'for/fold/derived))

(define-syntax (for*/data-frame stx)
  (for_/data-frame stx #'for*/fold/derived))
