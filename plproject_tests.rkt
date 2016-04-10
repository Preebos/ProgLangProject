#lang racket
(provide (all-defined-out))
(include "plproject.rkt")

; a test case that uses parts 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
;(define test1
;  (mupllist->racketlist
;   (eval-exp (call (call mupl-mapAddN (int 7))
;                   (racketlist->mupllist 
;                    (list (int 3) (int 4) (int 9)))))))

;test racketlist->mupllist and mupllist->racketlist
(racketlist->mupllist (list (int 3) (int 4) (int 9)))
(mupllist->racketlist (apair (int 3) (apair (int 4) (apair (int 9) (aunit)))))
(eval-exp (add (int 4) (int 5)))
;(ifgreater (int 1) (int 2) (int 3) (int 4))
(mupllist->racketlist (racketlist->mupllist (list (int 3) (int 4) (int 9))))
(ifgreater (int 1) (int 2) (int 3) (int 4))
