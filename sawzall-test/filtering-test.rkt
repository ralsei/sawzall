#lang racket
(require data-frame
         rackunit
         sawzall
         threading
         "test-data.rkt"
         "util.rkt")

;; super basic filter
(define where-1 (where woodland2 (site) (string=? site "c")))
(define where-1-result
  (row-df [site day catch]
          "c"   1   10
          "c"   2   20))

;; binding multiple variables
(define where-2 (where woodland2 (site day catch)
                       (and (= day 2) (= catch 20) (string=? site "c"))))
(define where-2-result
  (row-df [site day catch]
          "c"   2   20))

;; no matches
(define where-3 (where woodland2 (site) (string=? site "canada"))) ; not a real place
(define where-3-result
  (column-df [site #()]
             [day #()]
             [catch #()]))

;; yes
(define where-4 (where woodland2 (site) #t))
(define where-4-result woodland2)

;; more compound booleans
(define where-5 (where ball2 (first last) (or (string=? first "sam") (string=? last "ert"))))
(define where-5-result
  (row-df [first last  game goals]
          "sam"  "son" 1    0
          "bob"  "ert" 1    1
          "bob"  "ert" 2    3))

(module+ test
  ;; I think the error message data-frame provides is good enough here
  ;; But we always want to stick with this error message if we aren't rolling our own
  (check-exn
   exn:fail:data-frame?
   (thunk (where woodland2 (non-existent) #t)))

  (check data-frame~=? where-1 where-1-result)
  (check data-frame~=? where-2 where-2-result)
  (check data-frame~=? where-3 where-3-result)
  (check data-frame~=? where-4 where-4-result)
  (check data-frame~=? where-5 where-5-result))
