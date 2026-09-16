#lang racket

(define (sum12312 a b term next)
  (if (> a b) 0 (+ (term a) (sum12312 (next a) b term next))))

;(sum 1 5 sqr add1)

(define (product a b term next)
  (if (> a b) 1 (* (term a) (product (next a) b term next))))

;(product 1 3 sqr add1)

(define (accumulate2 op nv a b term next)
(if (> a b) nv
    (op (term a) (accumulate2 op nv (next a) b term next))))
(define (id x) x)
;(accumulate + 0 1 3 id add1)

(accumulate2 + 0 -3 5 (lambda (x) x) (lambda (x) (+ x 1)))

;___________________________________________________

;zad0

(define (sum2 pred? a b term)
  (if (> a b) 0
      (+ (sum2 pred? (add1 a) b term) (if (pred? a) (term a) 0))
      )
  )

(define (sum-even-squares a b)
  (sum2 even? a b sqr)
  )

(define (sum-odd-cubes a b)
  (sum2 odd? a b (lambda (x) (* x x x)))
  )

;zad1

(define (my-identity) (lambda (x) x))
(define (my-lambda f) (lambda (x) (f x)))
(define (negate-pred pred) (lambda (x) (not (pred x))))
(define (my-compose f g) (lambda (x) (f (g x))))

;zad2

(define (complex-procedure f g) (lambda (x y) (g (f x) (f y))))

;zad3

;(define (apply-n f n)
; (lambda (x) (if (= n 0 )
;                 x
;                  ((apply-n f (sub1 n)) (f x)))))


(define (apply-n f n)
  (define (helper left-over result)
    (if (> left-over n)
        result
        (helper (add1 left-over) (f result)))
    )
  (lambda (x) (helper 1 x))
  )

;_____________________________
(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))
;______________________________

;zad4

(define (cool-expression n)
  (accumulate + 0 2 n (lambda (x) (* x x x)) (lambda (x) (+ x 3)))
  )

;zad5

(define (factorial n)
  (accumulate * 1 1 n (lambda (x) x) add1)
  )

;zad6

(define (prime? n)
  (if (= n 1)
      #f
      (accumulate (lambda (x y) (and x y)) #t 2 (floor (sqrt n)) (lambda (x) (not (zero? (remainder n x)))) add1)
  ))

;zad7

(define (all? a b pred?)
  (accumulate (lambda (x y) (and x y)) #t a b pred? add1)
              )

;zad8

(define (any? a b pred?)
  (accumulate (lambda (x y) (or x y)) #f a b pred? add1)
  )

;zad9

(define (argmin f a b)
  (define minimum a)
  (accumulate (lambda (x y) (if (< (f x) (f y)) x y)) minimum (add1 a) b identity add1)
  )

;zad10

(define (count-digits n)
  (if (zero? n) 1
      (accumulate + 0 1 n (lambda (x) 1) (lambda (x) (* x 10)))
      ))

;zad11

(define (count-pairs a b n)
  (accumulate + 0 a b (lambda (x) (if (any? x b (lambda (y) (= (+ x y) n))) 1 0))  add1)
              )