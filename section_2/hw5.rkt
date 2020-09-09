;; Programming Languages, Homework 5

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

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist r)
  (if (pair? r)
      (apair (car r) (racketlist->mupllist (cdr r)))
      (aunit)))

(define (mupllist->racketlist e)
  (if (apair? e)
      (cons (apair-e1 e) (mupllist->racketlist (apair-e2 e)))
      '()))
;; Problem 2

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
  ;(println "e:")
 ; (println e)
 ; (println "env:")
 ; (println env)
  (cond [(var? e) (eval-under-env (envlookup env (var-string e)) env)]
        [(string? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
                    
        [(int? e) e]
       ; [(var? e) (eval-under-env (envlookup env (var-string e)) env)]
        [(mlet? e)
         (let ([v1 (eval-under-env (mlet-var e) env)]
               [v2 (eval-under-env (mlet-e e) env)]
               [body (mlet-body e)])
           (if (string? v1) 
               (eval-under-env body (cons (cons v1 v2) env))
               (error "MUPL mlet error " e)))]
        [(fun? e) (closure env e)]
        [(closure? e) e]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [a (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let* ([f-env (closure-env c)]
                      [f (closure-fun c)]
                      [name (fun-nameopt f)]
                      [formal (fun-formal f)]
                      [body (fun-body f)])
                 (if (equal? name #f)
                     (eval-under-env
                      (mlet formal a body) f-env)
                     (eval-under-env
                      (mlet formal a body) (cons (cons name f) f-env))))
               (error "MUPL call error " c env)))]
        [(apair? e)
         (let ([v1  (eval-under-env (apair-e1 e) env) ]
               [v2  (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env) ])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst error")))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd error")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0)
      e2
      e3))
(define (toapair a)
  (apair (car a) (cdr a)))

(define (r->m r)
  (if (pair? r)
      (apair (apair (car (car r)) (cdr (car r))) (racketlist->mupllist (cdr r)))
      (aunit)))
(define (mlet* lstlst e2)
  (let ([l (r->m lstlst)])
    (call (fun "f" "p"
               (ifaunit (var "p")
                        e2
                        (mlet (fst (fst (var "p")))
                              (snd (fst (var "p")))
                              (call (var "f") (snd (var "p"))))))
           l)))
             
        
(define (ifeq e1 e2 e3 e4)
  (ifgreater e1 e2
             e4
             (ifgreater e2 e1
                 e4
                 e3)))
        
            

;; Problem 4

;(define mupl-map
;  (fun #f "fp1"
;    (fun "f1" "fp2" ; fp2: (apair list acc)
;         (if (aunit? (fst (var "fp2")))
  ;           (snd  (var "fp2"))
  ;           (apair (call (var "fp1") (snd (fst  (var "fp2"))))
   ;                 (call (var "f1") (snd (var "fp2"))))))))
                  
              
      

;(define mupl-mapAddN 
;  (mlet "map" mupl-map
 ;       "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))


