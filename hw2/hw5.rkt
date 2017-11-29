#lang racket
(provide (all-defined-out))

;; definition of structures for MUPL programs
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (name arg body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp arg)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define racketlist->mupllist
  (λ (ls)
    (cond
      [(null? ls)
       (aunit)]
      [else
       (apair (car ls) (racketlist->mupllist (cdr ls)))])))

(define mupllist->racketlist
  (λ (ls)
    (cond
      [(aunit? ls)
       '()]
      [else
       (cons (apair-e1 ls) (mupllist->racketlist (apair-e2 ls)))])))

;; Problem 2

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(or (int? e) (closure? e) (aunit? e))
         e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater takes two constant numbers and two MUPL expression")))]
        [(fun? e)
         (closure env e)]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-arg e) env)])
           (if (closure? v1)
               (let ([environment (cons (cons (fun-arg (closure-fun v1)) v2) (closure-env v1))]
                     [function (fun-body (closure-fun v1))])
                 (if (fun-name (closure-fun v1))
                   (eval-under-env function (cons (cons (fun-name (closure-fun v1)) v1) environment))
                   (eval-under-env function environment)))
               (error "MUPL type error")))]
        [(apair? e)
                                         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error (format "MUPL fst takes only pair ~v" v))))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error (format "MUPL snd takes only pair ~v" v))))]
        [else (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([p (car lstlst)])
        (mlet (car p) (cdr p) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2 (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3)))))

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "fl" "list"
            (ifaunit (var "list")
                     (aunit)
                     (apair (call (var "f") (fst (var "list")))
                            (call (var "fl") (snd (var "list"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

