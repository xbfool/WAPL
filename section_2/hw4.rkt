
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ low stride) high stride))))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [else (list-ref xs (remainder n (length xs)))]))

(define (stream-for-n-steps s n)
  (if (= 0 n)
      '()
      (cons (car s) (stream-for-n-steps (cdr s) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (remainder x 5))
                              (cons (- x) (lambda() (f (+ x 1))))
                              (cons x (lambda() (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (= x "dan.jpg")
                              (cons x (lambda () (f ("dog.jpg"))))
                              (cons x (lambda () (f ("dan.jpg"))))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car x)) (lambda() (f (cdr x)))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x y n)
                (cons (cons (list-nth-mod x n) (list-nth-mod y n))
                      (lambda () (f (x y (+ 1 n))))))])
    (lambda () (xs ys 0))))