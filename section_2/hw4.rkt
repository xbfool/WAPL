
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ low stride) high stride))))

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
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (remainder x 5))
                              (cons (- x) (lambda() (f (+ x 1))))
                              (cons x (lambda() (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (equal? x "dan.jpg")
                              (cons x (lambda () (f "dog.jpg")))
                              (cons x (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda() (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x y n)
                (cons (cons (list-nth-mod x n) (list-nth-mod y n))
                      (lambda () (f x y (+ 1 n)))))])
    (lambda () (f xs ys 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (v vec n)
              (cond [(>= n (vector-length vec)) #f]
                    [else (let ([r (vector-ref vec n)])
                       (if (pair? r)
                           (if (equal? v (car r))
                             r
                             (f v vec (+ 1 n)))
                           (f v vec (+ 1 n))))]))])
    (f v vec 0)))

(define (cached-assoc xs n)
  (letrec ([vec (make-vector n #f)]
           [i 0]
           [f (lambda (v)
                (let ([r (vector-assoc v vec)])
                (if (equal? #f r)
                    (let ([r1 (assoc v xs)])
                      (if (equal? r1 #f)
                          #f
                          (let ([r2 (car r1)])
                            (vector-set! vec i r1)
                            (if (= i (- n 1))
                                (set! i 0)
                                (set! i (+ 1 i)))
                            r1)))
                    r)))])
    f))
               
                         
              
                  