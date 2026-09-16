#lang racket

(define (my-map f l)
  (if (null? l) '()
      (cons (f (car l)) (my-map f (cdr l)))))

(define (my-filter pred? l)
  (cond
    [(null? l) '()]
    [(pred? (car l)) (cons (car l) (my-filter pred? (cdr l)))]
    [else (my-filter pred? (cdr l))]
    )
  )

(define (my-foldr op nv l)
  (if (null? l) nv
      (op (car l) (my-foldr op nv (cdr l)))))

(define (my-foldl op nv l)
  (if (null? l) nv
      (my-foldl op (op nv (car l)) (cdr l))))

(define (maximum l)
  (foldr max (car l) (cdr l)))

(define (appy op l)
  (foldr op (car l) (cdr l)))

(define (my-length l)
  (if (null? l) 0
      (+ 1 (my-length (cdr l)))))

(define (my-length2 l)
  (foldr (lambda (_ r) (+ 1 r)) 0 l))

(define (my-append l1 l2)
 (if (null? l1) l2
     (cons  (car l1) (append (cdr l1) l2))))

(define (my-append2 l1 l2)
  (foldr cons l2 l1))

(define (snoc x l)
  (append l (list x)))

(define (my-reverse l)
  (if (null? l)  '()
      (append (my-reverse (cdr l) (list (car l))))))

(define (my-reverse2 l)
  (foldl cons '() l))

(define (my-list-tail l n) 
  (if (= n 0) l
      (my-list-tail (cdr l) (sub1 n))))

(define (my-list-ref l n)
  (if (zero? n) (car l)
      (my-list-ref (cdr l) (sub1 n))))

(define (my-member x l)
  (cond
    [(null? l) #f]
    [(= x (car l)) l]
    [else (my-member x (cdr l))]
    )
  )

(define (member* =? x l)
  (cond ((null? l) #f)
        ((=? x (car l)) l)
        (else (member* =? x (cdr l)))))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (add1 a) b))))

(define (accumulate a b op nv next term)
  (if (> a b) nv
      (accumulate (next a) b op (op nv (term a)) next term)))

(define (from-to2 a b)
  (accumulate a b append '() add1 list))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define 1+ (lambda (x) (add1 x)))