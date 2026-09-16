#lang racket

;зад 1
;Напишете процедура, която пресмята квадрата на дадено число.

(displayln "Zad1")
(define (square n) (* n n))

(= (square 5) 25)
(= (square -6) 36)

;зад 2
;Напишете процедура, която проверява дали дадено число е трицифено.

(displayln "Zad2")

(define (three-digit-number? n)
  (or (and (> n 99) (< n 1000))
      (and (< n -99) (> n -1000))
      )
  )

(equal? (three-digit-number? 125) #t)
(equal? (three-digit-number? -125) #t)
(equal? (three-digit-number? 56) #f)
(equal? (three-digit-number? 3456) #f)

;зад 3
;Напишете процедура, която намира последната цифра на дадено число

(displayln "Zad3")

(define (last-digit n) (remainder n 10))

(= (last-digit 176) 6)
(= (last-digit 0) 0)

;зад 4
;Напишете процедура, която закръгля число до три знака след десетичната запетая.

(displayln "Zad4")

(define (round-three-dig n) (/ (floor (* n 1000)) 1000))

(= (round-three-dig 12.345678) 12.345)

;зад 5
;Напишете процедура, която пресмята сумата на кубовете на две числа.

(displayln "Zad5")

;не използвайки вгадени функции
(define (cube-no-pow n) (* n n n))
(define (sum-cubes-no-pow x y) (+ (cube-no-pow x) (cube-no-pow y)))

(= (sum-cubes-no-pow 5 1) 126)
(= (sum-cubes-no-pow 10 50) 126000)

;използвайки вградена функция

(define (cube-pow n) (expt n 3))
(define (sum-cubes-pow x y) (+ (cube-pow x) (cube-pow y)))

(= (sum-cubes-pow 5 1) 126)
(= (sum-cubes-pow 10 50) 126000)

;зад 6
;Напишете процедура, която намира по-голямото от две числа.
(displayln "Zad6")

;използвайки вградена процедура
(define (my-max-built-in a b) (max a b))

(= (my-max-built-in 5 6) 6)

;използвайки if
(define (my-max-if a b) (if (< a b) b a))

(= (my-max-if -60 -15) -15)
(= (my-max-if 15 60) 60)
(= (my-max-if 60 15) 60)

;използвайки guards

(define (my-max-guard a b)
  (cond
    [ (< a b) b]
    [else a]
    )
  )

(= (my-max-guard 15 60) 60)
(= (my-max-guard 60 15) 60)

;зад 7
;Напишете предикат, който проверява дали две числа са различни на един ред без if.
(displayln "Zad7")

(define (not-equal-one-line? a b) (not (= a b)))

(equal? (not-equal-one-line? 5 2) #t)
(equal? (not-equal-one-line? 5 5) #f)

;зад 8
;Напишете предикат, който проверява дадено число n е между други две числа a и b.
(displayln "Zad8")

(define (between? a b n)
  (or (and (< a n) (< n b))
      (and (< n a) (< b n))
      )
  )

(equal? (between? 1 5 4) #t)
(equal? (between? 5 1 4) #t)
(equal? (between? 10 50 200) #f)
(equal? (between? 10 50 1) #f)

;зад 9
;Напишете предикат, който проверява дали една година е високосна.
(displayln "Zad9")

;на един ред с булеви оператори
(define (leap-year-one-line? n)
  (or (zero? (remainder n 400))
      (and (zero? (remainder n 4))
           (not (zero? (remainder n 100))))
      )
  )

(equal? (leap-year-one-line? 2000) #t)
(equal? (leap-year-one-line? 1900) #f)
(equal? (leap-year-one-line? 2004) #t)

;използвайки guards

(define (leap-year-guards?  n)
  (cond
    [(zero? (remainder n 400)) #t]
    [(zero? (remainder n 100)) #f]
    [(zero? (remainder n 4)) #t]
    [else #f]
    )
  )

(equal? (leap-year-guards? 2000) #t)
(equal? (leap-year-guards? 1900) #f)
(equal? (leap-year-guards? 2004) #t)

;зад 10
;Имплементирайте Алгоритъма на Евклид за намиране най-голям общ делител.
(displayln "Zad10")

(define (my-gcd x y)
  (cond
    [(zero? x) y]
    [(zero? y) x]
    [else (my-gcd y (remainder x y))]
    )
  )

(= (my-gcd 5 13) 1)
(= (my-gcd 13 1235) 13)

;зад 11
(displayln "Zad11")

(define (help-gosho breakfast+lunch)
  (define dinner (- 15 breakfast+lunch))
  (cond
    [(and (>= dinner 10) (<= dinner 15)) "Pizza"]
    [(>= dinner 5) "Doner"]
    [(>= dinner 3) "University cafeteria"]
    [else "Hungry"]
     )
  )

(equal? (help-gosho 0) "Pizza")
(equal? (help-gosho 7.5) "Doner")
(equal? (help-gosho 10.01) "University cafeteria")

;зад 12
(displayln "Zad12")

(define (help-students x y)
  (cond
    [(and (zero? x) (zero? y) "Center")]
    [(zero? x) "Ordinate"]
    [(zero? y) "Abscissa"]
    [(and (positive? x) (positive? y)) "1 Quadrant"]
    [(positive? x) "4 Quadrant"]
    [(positive? y) "2 Quadrant"]
    [else "3 Quadrant"]
    )
  )

(equal? (help-students 0 0) "Center")
(equal? (help-students 5 0) "Abscissa")
(equal? (help-students 0 4) "Ordinate")
(equal? (help-students 1 1) "1 Quadrant")
(equal? (help-students -2 -4) "3 Quadrant")

;зад 13
(displayln "Zad13")

(define (mirror? n)
  (define ones (remainder n 10))
  (define tens (remainder (quotient n 10) 10))
  (define hundreds (remainder (quotient n 100) 10))
  (define thousands (quotient n 1000))
  (and (= ones thousands) (= tens hundreds))
  )
    
(equal? (mirror? 2332) #t)
(equal? (mirror? 1111) #t)
(equal? (mirror? 5005) #t)
(equal? (mirror? 1234) #f)

;зад 14
;(displayln "Zad14")
;skip

;зад 15
(displayln "Zad15")

(define (help-preslava hours minutes songs)
  (define min-start (+ (* hours 60) minutes))
  (define min-end (+ min-start (* songs 3)))
  (cons (quotient min-end 60) (remainder min-end 60)))

(equal? (help-preslava 2 0 10) '(2 . 30))
(equal? (help-preslava 2 30 15) '(3 . 15))
