#lang scribble/manual
@(require scribble/example (for-label racket data-frame sawzall threading))

@(define ev
   (let ([eval (make-base-eval)])
     (eval '(require data-frame threading sawzall))
     eval))

@title{Sawzall: A grammar for chopping up data}
@author{@(author+email "Hazel Levine" "hazel@knightsofthelambdacalcul.us")}

@defmodule[sawzall]

Pain. Agony, even. Suffering, perhaps.
