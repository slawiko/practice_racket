#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; ===Helpers===
(define nats
  (letrec ([f (λ (x)
                (cons x (λ () (f (+ x 1)))))])
    (λ () (f 1))))

(define stream-generator
  (λ (x)
    (λ ()
      (cons x (stream-generator x)))))
; =============

(define sequence
  (λ (low high stride)
    (if (<= low high)
        (cons low (sequence (+ low stride) high stride))
        '())))

(define string-append-map
  (λ (xs suffix)
    (map
      (λ (str)
        (string-append str suffix))
      xs)))

(define list-nth-mod
  (λ (xs n)
    (cond
      [(null? xs) (error "list-nth-mod: empty list")]
      [(negative? n) (error "list-nth-mod: negative number")]
      [else
        (car
          (list-tail
            xs
            (remainder n (length xs))))])))

(define stream-for-n-steps
  (λ (s n)
    (if (> n 0)
        (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))
        '())))

(define stream-map
  (λ (s f)
    (λ ()
      (cons (f (car (s))) (stream-map (cdr (s)) f)))))

(define combine-streams
  (λ (s1 s2 f)
    (λ ()
      (cons (f (car (s1)) (car (s2))) (combine-streams (cdr (s1)) (cdr (s2)) f)))))

(define skip-first-n
  (λ (s n)
    (if (> n 0)
        (skip-first-n (cdr (s)) (- n 1))
        s)
    ))

(define (funny-number-stream)
  ((stream-map nats (λ (e)
                  (if (= (remainder e 5) 0)
                      (* e -1)
                      e)))))

(define (squares s)
  (stream-map s (λ (x) (* x x))))

(define (taylor x)
  (combine-streams nats (stream-generator x) (λ (n x)
                                               (/ (* (expt -1 (- n 1)) (expt x n)) n))))

