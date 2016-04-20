;; CS 4003: Programming Languages, Team Project

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Part 1 - Warm-up

(define (racketlist->mupllist rlist)
   (cond
     [(null? rlist) (aunit)]
     [#t (apair (car rlist) (racketlist->mupllist (cdr rlist)))]))

(define (mupllist->racketlist mlist)
   (cond
     [(equal? (int 1) (eval-exp (isaunit mlist))) null]
     [#t (cons (eval-exp (fst mlist)) (mupllist->racketlist (snd mlist)))]))


;; Part 2 - Implementing the language

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]

        ; fun
        [(fun? e) (closure e env)]
        

        ; ifgreater
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]
               [v3 (eval-under-env (ifgreater-e3 e) env)]
               [v4 (eval-under-env (ifgreater-e4 e) env)])
               (cond
                 [(and
                   (int? v1)
                   (int? v2))
                  (cond
                    [(> (int-num v1) (int-num v2)) v3]
                    [#t v4])]
                 [#t (error "Both e1 and e2 must be ints")]))]

        ; mlet
        [(mlet? e)
         (let* ([v (mlet-var e)]
               [val (eval-under-env (mlet-e e) env)]
               [newEnv (cons env (list v val))]
               [body (mlet-body e)])
           (eval-under-env body newEnv))]
           
        ; call
        [(call? e)
         (let ([clo (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (not (closure? clo))
             (error "First argument must be a closure")
             (let* ([en     (closure-env clo)]
                    [f      (closure-fun clo)]
                    [name   (fun-nameopt f)]
                    [formal (fun-formal  f)]
                    [body   (fun-body    f)])
               (if (not name)
                   (eval-under-env body en)
                   (eval-under-env body (cons en (list name clo)))))))]            

        ; apair
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
               (apair v1 v2))]
        
        ; fst and snd
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
               (cond
                 [(apair? v1) (apair-e1 v1)]
                 [#t (error "Expression is not a pair")]))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
               (cond
                 [(apair? v1) (apair-e2 v1)]
                 [#t (error "Expression is not a pair")]))]
        
        ; isaunit
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
               (cond
                 [(aunit? v1) (int 1)]
                 [#t (int 0)]))]

        ; closure
        [(closure? e)
         (let ([f (eval-under-env (closure-fun e) env)])
           (cond
             [(fun? f) e]
             [#t (error "second arg should be a function")]))]
        
        ;; DO NOT CHANGE else case
        [#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Part 3 - Expanding the language

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond
    [(null? lstlst) e2]
    [#t (let ([hd (car lstlst)])
          (mlet (car hd) (cdr hd) (mlet* (cdr lstlst) e2)))]))

(define (ifeq e1 e2 e3 e4) (if (= (int-num e1) (int-num e2)) e3 e4))

;; Part 4 - Using the language

(define mupl-map "change")


(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

