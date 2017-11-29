#lang racket

;; Some trivial tests are allredy done for you. You should add more tests here.
;; Although tests won't be graded, make sure that you solution is tested well.

(require "hw4_provided.rkt")
(require rackunit)

;; Helper functions
(define ones (stream-generator 1))
(define twos (stream-generator 2))

(define tests
  (test-suite
   "Sample tests for HW4"
    ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")
   (check-equal? (sequence 0 5 2) (list 0 2 4) "Sequence test")
   (check-equal? (sequence 3 2 1) '() "Sequence test")

   ; string-append-map test
   (check-equal? (string-append-map 
                 (list "foo" "bar") 
                 ".rkt") '("foo.rkt" "bar.rkt") "string-append-map test")
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")
   (check-exn (regexp "list-nth-mod: negative number") (lambda () (list-nth-mod (list 0 1 2 3 4) -1)) "list-nth-mod test2")
   (check-exn (regexp "list-nth-mod: empty list") (lambda () (list-nth-mod '() 1)) "list-nth-mod test2")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 1) (list 1) "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps (lambda () (cons 2 twos)) 4) (list 2 2 2 2) "stream-for-n-steps test")

   ; stream-map test
   (check-equal? (stream-for-n-steps (stream-map ones (lambda (x) (+ x 1))) 2) (list 2 2) "stream-map test")
   (check-equal? (stream-for-n-steps (stream-map twos (lambda (x) (+ x 1))) 5) (list 3 3 3 3 3) "stream-map test")

   ; combine-stream test
   (check-equal? (stream-for-n-steps (combine-streams ones twos +) 2) (list 3 3) "combine-streams test")

   ; skip-first-n test
   (check-equal? (stream-for-n-steps (skip-first-n nats 5) 1) (list 6) "skip-first-n test")
      
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")
   (check-equal? (stream-for-n-steps (skip-first-n funny-number-stream 5) 3) (list 6 7 8) "funny-number-stream and skip-first-n test")

   ; squares test
   (check-equal? (stream-for-n-steps (squares ones) 3) (list 1 1 1) "squares test")
   
   ; taylor test
   (check-= 0.095 (apply + (stream-for-n-steps (taylor 0.1) 10)) 0.005 "taylor test")
   
   ; cycle-lists test
   ;(check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
   ;              "cycle-lists test")
   
   ; vector-assoc test
   ;(check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")
   
   ; cached-assoc tests
   ;(check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
     
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
