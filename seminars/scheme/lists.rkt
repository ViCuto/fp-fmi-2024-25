#lang racket

;zad1
(define (sort-list l) (lambda (f) (sort l f)))

;zad2
(define (my-length l)
  (if (null? l) 0
      (add1 (my-length (cdr l)))
      )
  )

;zad3
(define (get-smallest l)
  (foldr min (car l) (cdr l)))

(define (get-smallest2 l)
  (apply min l))

;zad4
(define (remove-first n l)
  (cond
    [(null? l) l]                           ; Ако списъкът е празен, върни го
    [(equal? n (car l)) (cdr l)]                  ; Ако първият елемент е равен на n, върни опашката на списъка
    [else (cons (car l) (remove-first n (cdr l)))] ; Ако не е равен, продължи с рекурсията върху опашката
  )
)


(define (remove-all n l)
  (cond
    [(null? l) l]
    [(equal? n (car l)) (remove-all n (cdr l))]
    [else (cons (car l) (remove-all n (cdr l)))]
    )
  )

;zad5

(define (num-to-xs n)
  (cond
    [(< n 10)  (list n)]
    [else (append (num-to-xs (quotient n 10)) (list (remainder n 10)))]
    )
  )

(define (xs-to-num l)
  (define (helper current result)
    (cond
      [(null? current) result]
      [else (helper (cdr current) (+ (car current) (* result 10)))]
      )
    )
  (if (null? l) 0 (helper l 0))
  )

;zad6

(define (set-union l1 l2)
  (sort (remove-duplicates (append l1 l2)) <))

;zad7
(define (my-reverse-foldl l)
  (foldl cons '() l)) 

;zad8
(define (kth-max-negative l)
  (lambda (k) (list-ref (sort (remove-duplicates (filter negative? l)) >) (sub1 k))))

;zad9

(define (insert-at n index l)
  (append (take l (sub1 index)) (cons n(drop l (sub1 index)))))

;zad10

(define (concat l1 l2)
  (if (null? l1) l2
      (cons (car l1) (concat (cdr l1) l2))
      )
  )

;zad11
(define (prime? n)
  (define (helper cur)
    (cond
    [(< (floor (sqrt n)) cur) #t]
    [(zero? (remainder n cur)) #f]
    [else (helper (add1 cur))]
    ))
  (if (= n 1) #f (helper 2))
  )

(define (factorise n)
  (define (helper cur-list cur-number prime-number)
    (cond
      [(prime? cur-number) (append cur-list (list cur-number))]
      [(and (prime? prime-number) (zero? (remainder cur-number prime-number))) (helper (append cur-list (list prime-number)) (/ cur-number prime-number) prime-number)]
      [else (helper cur-list cur-number (add1 prime-number))]
      )
    )
  (helper '() n 2)
  )

;zad13

(define (get-sublist a b lst)
  (take (drop lst a) (- b a -1)))

;zad14

(define (count-occurrences l1 l2)
  (define ll1 (length l1))  
  (define (helper result current)
    (cond
      [(< (length current) ll1) result]  
      [(equal? l1 (take current ll1)) (helper (add1 result) (cdr current))]  
      [else (helper result (cdr current))]))  
  (helper 0 l2))

;zad15

(define (ordered? l)
  (define (helper current result)
    (if (< (length current) 2) 
        result
        (helper (cdr current) (and result (< (car current) (cadr current))))))
  (helper l #t))
     
