#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root t) (car t))
(define (left t) (cadr t))
(define (right t) (caddr t))
;zad1

(define (inorder t)
  (cond
    [(null? t) '()]
    [else (append (inorder (left t)) (list (root t)) (inorder (right t)))]
    )
  )

(define (preorder t)
  (if (null? t) '()
      (append (list (root t)) (preorder (left t)) (preorder (right t)))))

(define (postorder  t)
  (if (null? t) '()
      (append (postorder (left t)) (postorder (right t)) (list (root t)))))

;zad2

(define (size t)
  (if (null? t) 0
      (+ 1 (size (left t)) (size (right t)))))

;zad3

(define (height t)
  (if (null? t) 0
      (+ 1 (max (height (left t)) (height (right t))))))

;zad4

(define (balanced? t)
  (if (null? t) #t
      (and (<= (abs (- (height (left t)) (height (right t)))) 1) (balanced? (left t)) (balanced? (right t)))))

;zad5

(define (perfectly-balanced? t)
  (if (null? t) #t
       (and (<= (abs (- (size (left t)) (size (right t)))) 1) (perfectly-balanced? (left t)) (perfectly-balanced? (right t)))))

;zad7

(define (leaf? t) (and (not (null? t)) (null? (left t)) (null? (right t))))

(define (leaves t)
  (cond
    [(null? t) '()]
    [(leaf? t) (list (root t))]
    [else (append (leaves (left t)) (leaves (right t)))]))

;zad8


(define (at-level n t)
  (cond
    [(null? t) '()]
    [(zero? n) (list (root t))]
    [else (append (at-level (sub1 n) (left t)) (at-level (sub1 n) (right t)))]))

;zad9

(define (map2 f t)
  (if (null? t) '()
      (list (f (root t)) (map2 f (left t)) (map2 f (right t)))))

;zad10

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define (zero-rows xss)
  (cond
    [(null? xss) '()]
    [else (if (search zero? (car xss)) (cons (map (lambda (x) 0) (car xss)) (zero-rows (cdr xss)))
                                             (cons (car xss) (zero-rows (cdr xss))))]))


(define (make-tree root left right)
  (list root left right))
(define (make-leaf t) (make-tree t '() '()))

(define (flip-tree t)
  (make-tree (root t) (right t) (left t)))

(define t1 (make-tree 1 (make-leaf 2) (make-leaf 3)))
(define t2 (make-tree 1 (make-leaf 3) (make-leaf 2)))

(equal? t1 (flip-tree t2))

(define (alist-values alist)
  (map cdr alist))

(alist-values '((1 . 2) (1 . 2) (1 . 2)))

(define (make-alist f lst)
  (map (lambda (x) (cons x (f x))) lst))
(define a1 (make-alist identity '(1 2 3 4)))

(define (add-assoc key value alist)
  (cons (cons key value) alist))

(define (del-assoc key alist)
  (cond
    [(null? alist) '()]
    [(equal? key (car (car alist))) (cdr alist)]
    [else (cons (car alist) (del-assoc key (cdr alist)))]))
