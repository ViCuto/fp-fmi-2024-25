#lang racket

;zad 1

(define (fib-recursive n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fib-recursive (- n 1)) (fib-recursive (- n 2)))]
    )
  )

(define (fib-iterative n)
  (define (iter i fi fi-1)
    (if (= i n) fi
        (iter (+ i 1) (+ fi fi-1) fi)))
  (if (= n 0) 0
      (iter 1 1 0)))

;zad2

(define (help-galena new-views lost-views desired-views)
  (define next-day-views (+ new-views lost-views))
  (define (iter day views)
    (if (<= desired-views views)
        day
        (iter (+ day 1) (+ views next-day-views))))
  (iter 1 0))

;zad3

(define (road-to-logprog n)
  (cond
    [(= n 1) 1]
    [(= n 2) 2]
    [else (+ (road-to-logprog (- n 2)) (road-to-logprog (- n 1)))]
    )
  )

;zad4

(define (rev x)
  (define (helper result cur-number)
    (if (zero? cur-number) result
        (helper (+ (* result 10) (remainder cur-number 10)) (quotient cur-number 10))))
  (helper 0 x)
  )

;zad5

(define (my-recursive-pow x n)
  (cond
    [(= n 0) 1]
    [(< n 0) (/ 1 (my-recursive-pow x (- n)))]
    [(even? n) (sqr (my-recursive-pow x (quotient n 2)))]
    [else (* x (my-recursive-pow x (sub1 n)))]
    )
  )

;zad6

(define (prime? n)
  (define (helper current)
    (cond
      [(> current (sqrt n)) #t]
      [(zero? (remainder n current)) #f]
      [else (helper (+ current 1))]
      )
    )
  (and (not (= n 1)) (helper 2))                
)

;zad7

(define (increasing-digits? n)
  (cond
    [(< n 10) #t]
    [(< (remainder n 10) (remainder (quotient n 10) 10)) #f]
    [else (increasing-digits? (quotient n 10))]
    )
  )

;zad8

(define (palindrome? n) (= n (rev n)))

(define (palindromes-between a b)
  (define (helper sum i)
    (if (< b i) sum
        (helper (if (palindrome? i)
                    (+ sum 1)
                    sum)
                (+ i 1))))
  (if (< b a) (palindromes-between b a) (helper 0 a))
  )

;zad9

(define (contain-digit? n a)
  (cond
    [(= n 0) #f]
    [(= (remainder n 10) a) #t]
    [else (contain-digit? (quotient n 10) a)]
    )
  )

(define (sum-first-special-numbers n d)
  (define (helper number sum left)
    (cond
      [(= left 0) sum]
      [(and (prime? number) (contain-digit? number d)) (helper (+ number 1) (+ sum number) (sub1 left))]
      [else (helper (+ number 1) sum left)]
      )
    )
  (helper 2 0 n)
  )

;zad10

(define (automorphic? n)
  (define dn (* n n))
  (define (helper cur-n cur-dn)
  (cond
    [(= cur-n 0) #t]
    [(not(= (remainder cur-n 10) (remainder cur-dn 10))) #f]
    [else (helper (quotient cur-n 10) (quotient cur-dn 10))]
    ))
  (helper n dn)
  )

;zad11

(define (count-of-digits n)
  (define (helper sum current)
    (if (zero? current)
        sum
        (helper (add1 sum) (quotient current 10)))
    )
  (helper 0 n)
  )

(define (get-last-n-digits number n)
  (remainder number (expt 10 n)))

(define (subnumber? x y)
  (define number-digits (count-of-digits x))
  (define (helper current)
    (cond
      [(= x (get-last-n-digits current number-digits)) #t]
      [(< current x) #f]
      [else (helper (quotient current 10))]
      )
    )
  (helper y)
  )

;zad12

(define (all-digits-same? n)
  (define digit (remainder n 10))
  (define (helper current)
    (cond
      [(zero? current) #t]
      [(not (= digit (remainder current 10))) #f]
      [else (helper (quotient current 10))]
      )
    )
  (helper n)
  )

(define (sum-digits n)
  (define (helper current sum)
    (if (zero? current)
        sum
        (helper (quotient current 10) (+ sum (remainder current 10)))
        )
    )
  (helper n 0)
  )

(define (digital-root n)
  (define sum (sum-digits n))
  (cond
    [(< sum 10) sum]
    [(all-digits-same? n) (remainder n 10)]
    ;[(< sum 10) sum]
    [else (digital-root sum)]
    )
  )

;zad13

(define (remove-first-occurrence n d)
  (cond
    [(= n d) 0]
    [(< n 10) n]
    [(= (remainder n 10) d) (quotient n 10)]
    [else (+ (* 10 (remove-first-occurrence (quotient n 10) d)) (remainder n 10))]
    )
  )

;zad14

(define (max-digit-in-number n)
  (if (< n 10)
      n
      (max (remainder n 10) (max-digit-in-number (quotient n 10)))
      )
  )

(define (sort-n n)
  (define (helper current result)
    (if (< current 10)
        (+ (* result 10) current)
         (let ((max-digit (max-digit-in-number current))) (helper (remove-first-occurrence current max-digit) (+ (* result 10) max-digit)))
        )
    )
  (helper n 0)
  )

;zad15
      