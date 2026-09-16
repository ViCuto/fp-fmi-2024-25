#lang racket
(define bobo-trie
    (list
        (cons #\b
            (list
                (cons #\a
                    (list
                        (cons #\b
                            (list
                                (cons #\a
                                    (list))))))
                (cons #\o
                    (list
                        (cons #\b
                            (list
                                (cons #\o
                                    (list))))))))
        (cons #\d
              (list
                (cons #\a
                    (list))))))
;strings
(define (string->list str1)
  (define str-len (string-length str1))
  (define (helper count lst)
    (if (< count 0)  
        lst
        (helper (sub1 count) (cons (string-ref str1 count) lst))))
  (helper (sub1 str-len) '()))  


(define (str-sub str a b)
  (define (helper current result)
    (if (>= current b)
        result
        (helper (add1 current) (string-append result (string (string-ref str current))))))
  (helper a ""))
           

(define (str-chr? str c)
  (define (helper index)
    (if (>= index (string-length str))
        #f 
        (if (equal? (string-ref str index) c)
            #t  
            (helper (+ index 1))))) 
  (helper 0))  

; (str-chr? "hello" #\o)


;prefix-trie

(define (trie-words trie)
  (define (helper node path)
    (if (null? node)
        (list (list->string path))  
        (apply append
               (map (lambda (entry)
                      (let ((char (car entry))
                            (sub-trie (cdr entry)))
                        (helper sub-trie (append path (list char))))) 
                    node))))
  (helper trie '()))  

(trie-words bobo-trie)

