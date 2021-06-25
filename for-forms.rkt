#lang racket/base
(require (for-syntax racket/base
                     syntax/for-body
                     syntax/parse)
         data-frame
         "display.rkt"
         "facet.rkt")
(provide for/data-frame for*/data-frame)

(define-syntax (for/data-frame stx)
  (syntax-parse stx
   [(_ (column:id ...) clauses body ... tail-expr)
    #:with original stx
    #:with ((pre-body ...) (post-body ...)) (split-for-body stx #'(body ... tail-expr))
    #'(for/fold/derived original
        ([current-df #f])
        clauses
        pre-body ...

        (define single-row-df (make-data-frame))
        (for ([name (in-list (list (symbol->string 'column) ...))]
              [val (in-vector (call-with-values (λ () post-body ...) vector))])
          (df-add-series! single-row-df (make-series name #:data (vector val))))
        (if current-df (unfacet current-df single-row-df) single-row-df))]))

(define-syntax (for*/data-frame stx)
  (syntax-parse stx
   [(_ (column:id ...) clauses body ... tail-expr)
    #:with original stx
    #:with ((pre-body ...) (post-body ...)) (split-for-body stx #'(body ... tail-expr))
    #'(for*/fold/derived original
        ([current-df #f])
        clauses
        pre-body ...

        (define single-row-df (make-data-frame))
        (for ([name (in-list (list (symbol->string 'column) ...))]
              [val (in-vector (call-with-values (λ () post-body ...) vector))])
          (df-add-series! single-row-df (make-series name #:data (vector val))))
        (if current-df (unfacet current-df single-row-df) single-row-df))]))
