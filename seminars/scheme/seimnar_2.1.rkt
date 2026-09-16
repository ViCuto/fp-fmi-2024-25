#lang racket

(define (add-1 x)
 (+ x 1))

(define (apply-with-2 f x)
  (f x 2))

(define (o f g)
  (define (comp-f-g x)
    (f (g x)))

  comp-f-g)

(define (double x)
  (* 2 x))

(define add2-1 (lambda (x)
                 (+ 1 x)))

(define (o2 f g)
  (lambda (x) (f (g x))))

(define (repeated n f x)
  (if (= n 0)
      x
      (repeated (- n 1) f (f x))))

(define (repeat f n)
  (lambda (x)
    (if (= n 0)
    x
    ((repeat f (- n 1)) (f x)))))

(define (repeat-2 f n)
  (lambda (x) repeated n f x))

(define (repeat-3 f n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) (f (repeat-3 (- n 1) f) x)))) 

(define (repeat-4 n f)
  (repeated n (lambda (g) (o f g)) (lambda (x) x)))

(define (accumulate-n op init begin end)
  (if (> begin end)
      init  
      (op begin (accumulate-n op init (+ begin 1) end))))

(define (accumulate2-n op init f begin end)
  (if (> begin end)
      init  
      (op (f begin) (accumulate2-n op init f (+ 1 begin) end))))

(define (fact x)
  (accumulate-n * 1 1 x))

(define (count p a b)
  (accumulate2-n
   + ; op
   0 ; init
   (lambda (x) (if (p x) 1 0 )) ; f
   a b))

(define (my-or x y) (if x #t
                      (if y #t #f)))

(define (exists? p a b)
  (accumulate2-n
   my-or
   #f
   p
   a b))

(define (repeat5 n f)
  (accumulate-n
   o
   (lambda (x) x)
   (lambda (x) f)
   1 n))
(define (lenght list)
  (if (empty? list)
      0
      (+ 1 (lenght (cdr list)))))