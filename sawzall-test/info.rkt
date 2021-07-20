#lang info

(define collection "sawzall-test")

(define test-omit-paths '("./info.rkt"))
(define test-responsibles '((all hazel@knightsofthelambdacalcul.us)))

(define pkg-desc "Tests for Sawzall")
(define version "1.0")
(define deps '("base"
               "data-frame"
               "rackunit-lib"
               "sawzall-lib"
               "threading-lib"))
