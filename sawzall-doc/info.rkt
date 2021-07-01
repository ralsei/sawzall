#lang info

(define collection "sawzall-doc")
(define scribblings '(("sawzall.scrbl" ())))

(define pkg-desc "Documentation for Sawzall")
(define version "0.1")
(define deps '("base"))
(define build-deps '("data-frame"
                     "racket-doc"
                     "sawzall-lib"
                     "scribble-lib"
                     "threading-lib"))
