#lang info

(define collection "sawzall-doc")
(define scribblings '(("sawzall.scrbl" (multi-page))))

(define pkg-desc "Documentation for Sawzall")
(define version "1.0")
(define deps '("base"))
(define build-deps '("data-frame"
                     "racket-doc"
                     "sawzall-lib"
                     "scribble-lib"
                     "threading-lib"
                     "threading-doc"))
