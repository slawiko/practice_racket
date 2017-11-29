#lang racket

;; Some trivial tests are allredy done for you. You should add more tests here.
;; Although tests won't be graded, make sure that you solution is tested well.

(require "hw5.rkt")
(require rackunit)

(define tests
  (test-suite
   "Sample tests for HW5"
   
   ;; racketlist to mupllist test
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   
   ;; mupllist to racketlist test
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "mupllist->racketlist test")

   ;; int test
   (check-equal? (eval-exp (int 3)) (int 3) "int test")

   ;; closure test
   (check-equal? (eval-exp (closure null (fun #f "x" (var "x")))) (closure null (fun #f "x" (var "x"))) "closure test")
   
   ;; add test
   (check-equal? (eval-exp (add (int 4) (int 3))) (int 7) "add test")

   ;; ifgreater test
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   
   ;; fun test
   (check-equal? (eval-exp (fun #f "x" (var "x"))) (closure null (fun #f "x" (var "x"))) "fun test")
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")

   ;;fst test
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "fst test")
   
   ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 15))) (add (var "x") (var "y")))) (int 25) "mlet* test")
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 2) (int 1) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 2) (int 2) (int 3) (int 4))) (int 3) "ifeq test")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (apair (int 8) (aunit))))) 
                 (apair (int 8) (apair (int 15) (aunit))) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist (eval-exp (call (call mupl-mapAddN (int 7))
                                                       (racketlist->mupllist(list (int 3) (int 4) (int 9))))))
                 (list (int 10) (int 11) (int 16)) "combined test")
   
   ))

(require rackunit/text-ui)
(run-tests tests)
