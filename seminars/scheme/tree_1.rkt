#lang racket

(define (make-alist f keys)
  (map (lambda (x) (cons x (f x))) keys))

(define (keys alist) (map car alist))

(define (search p l)
(and (not (null? l))
(or (p (car l)) (search p (cdr l)))))
;_______________________
;TREE

(define t '(1 (2 () ())
(3 (4 () ())
(5 () ()))))

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define empty-tree '())

(define (make-tree root left right) (list root left right))
(define (make-leaf x) (make-tree x empty-tree empty-tree))

;(define t1 (make-tree 1 (make-tree 2 3 4) (make-tree 4 (make-leaf 6) 9)))
;(define t2 (make-tree 2 3 4))

(define root-tree car)

(define left-tree cadr)

(define right-tree caddr)

(define empty-tree? null?)

(define (depth-tree t)
  (if (empty-tree? t) 0
      (add1 (max (depth-tree (left-tree t))
                 (depth-tree (right-tree t)))
            )
      )
  )

(define (memv-tree x t)
(cond ((empty-tree? t) #f)
((eqv? x (root-tree t)) t)
(else (or (memv-tree x (left-tree t))
(memv-tree x (right-tree t))))))

(define (path-tree x t)
  (cond
    [(empty-tree? t) #f]
    [(equal? x (root-tree t)) (list x)]
    [else (cons (root-tree t)
                (or (path-tree x (left-tree t))
                    (path-tree x (right-tree t))))]
    )
  )

(define t1 (make-tree 1 (make-tree 2 (make-leaf 3) (make-leaf 4))
                      (make-tree 5 (make-leaf 6) (make-leaf 7))))
(define t2(make-tree 1 (make-tree 2 (make-leaf 3) (make-leaf 4))
                      (make-tree 5 (make-leaf 6) (make-leaf 7))))
(define t3 (make-tree 2 (make-leaf 3) (make-leaf 4)))


;;atom

(define (atom? l) (and (not (null? l)) (not (pair? l))))


