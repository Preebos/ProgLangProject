#lang racket
(require "plproject.rkt")

; a test case that uses parts 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
;(define test1
;  (mupllist->racketlist
;   (eval-exp (call (call mupl-mapAddN (int 7))
;                   (racketlist->mupllist 
;                    (list (int 3) (int 4) (int 9)))))))

;;;; tests for working parts:

; expect (apair (int 3) (apair (int 4) (apair (int 9) (aunit))))
(racketlist->mupllist (list (int 3) (int 4) (int 9)))

; expect (list (int 3) (int 4) (int 9))
(mupllist->racketlist (apair (int 3) (apair (int 4) (apair (int 9) (aunit)))))

; expect (int 3)
(eval-exp (add (int 2) (int 1)))
; expect (int 102)
(eval-exp (add (int 50) (int 52)))

; expect (int 4)
(eval-exp (ifgreater (int 1) (int 2) (int 3) (int 4)))
; expect (int 3)
(eval-exp (ifgreater (int 9) (int 2) (int 3) (int 4)))

; expect (apair (int 1) (int 2))
(eval-exp (apair (int 1) (int 2)))
; expect (apair (int 1) (apair (int 2) (aunit)))
(eval-exp (apair (int 1) (apair (int 2) (aunit))))

; expect (int 1)
(eval-exp (fst (apair (int 1) (int 2))))
; expect (int 1)
(eval-exp (fst (apair (int 1) (apair (int 2) (int 3)))))

; expect (int 2)
(eval-exp (snd (apair (int 1) (int 2))))
; expect (apair (int 2) (int 3))
(eval-exp (snd (apair (int 1) (apair (int 2) (int 3)))))
; expect (apair (int 2) (aunit))
(eval-exp (snd (apair (int 1) (apair (int 2) (aunit)))))

; expect (int 1)
(eval-exp (isaunit (aunit)))
; expect (int 0)
(eval-exp (isaunit (int 1)))

; expect (int 2)
(eval-exp (ifaunit (aunit) (int 2) (int 3)))
; expect (int 3)
(eval-exp (ifaunit (int 1) (int 2) (int 3)))


;;;; tests for non-working parts:

; expect (int 2)
;(eval-exp (ifeq (int 1) (int 1) (int 2) (int 3)))
; expect (int 3)
;(eval-exp (ifeq (int 1) (int 2) (int 2) (int 3)))










