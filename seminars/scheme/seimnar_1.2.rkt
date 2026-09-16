#lang racket

( define (avg x y) (/(+ x y) 2) )


( define (recip x) (if (= x 0)
                        #f
                        (/ 1 x)))

( define (myNot x) (if x
                      #f
                      #t))

( define (and x y) (if x
                       (if y #t #f)
                       #f))

( define (or x y) (if x #t
                      (if y #t #f)))

( define (xor x y) (if x
                       (if y
                           #f
                           #t)
                       (if y
                           #t
                           #f)))

( define (fact n) (if (= n 0)
                      1
                      (* n (fact (- n 1)))))

( define (fib n) (if (= n 1)
                     0
                     (if (= n 2)
                      1
                      (+ (fib (- n 1)) (fib (- n 2))))))

( define (count-digits n) (if (= n 0)
                              0
                              (+ (remainder n 10) (count-digits (quotient n 10)))))

( define (last-digit n) (remainder n 10))

( define (first-digit n)  1)

( define (palindrome? base n) 1 )

( define (factMemory n result) (if (= n 0)
                                 result
                                 (factMemory (- n 1) (* result n))))

