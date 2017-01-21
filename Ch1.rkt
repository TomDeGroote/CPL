#lang racket

;; Function to check if a value is in a given function
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0) (in-S? (- n 3))
            #f))))

;; Function to get the length of a list
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

;; Function to get the nth element of a list
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

;; Function to report when a list is too short
(define report-list-too-short
  (lambda (n)
    (error 'nth-element
           "List too short by ~s elements.~%" (+ n 1))))

;; Function that removes the first symbol in a list that is equal to the given list
(define remove-first
  (lambda (symbol lst)
    (if (null? lst)
        '()
        (if (eqv? (car lst) symbol)
            (cdr lst)
            (cons (car lst) (remove-first symbol (cdr lst)))))))

;; 1.15 Function that returns a list with x copies of n
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))

;; 1.16 Function that returns a list with each 2-list element reversed (car (cdr (car lst))) (car (car lst))
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car (cdr (car lst))) (car (car lst))) (invert (cdr lst))))))

;; 1.17 Function that wraps brackets around each top level element of a list
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst)) (down (cdr lst))))))

;; 1.19 Function that replaces nth element by x
(define list-set
  (lambda (lst n x)
    (if (zero? n)
        (cons x lst)
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

;; 1.21 Function that returns the cartesian product of the two given lists
(define product
  (lambda (lst1 lst2)
    (if (null? lst1)
        '()
        (append (product-element (car lst1) lst2) (product (cdr lst1) lst2)))))

(define product-element
  (lambda (x lst)
    (if (null? lst)
        '()
        (cons (list x (car lst)) (product-element x (cdr lst))))))

;; 1.29 Sort a list of integers from small to big, helper functions are changed to work for 1.30 as well
(define sort
  (lambda (loi)
    (sort-result loi '() <)))

(define sort-result
  (lambda (loi res pred)
    (if (null? loi)
        res
        (sort-result (cdr loi) (add-sorted (car loi) res pred) pred))))

(define add-sorted
  (lambda (x lst pred)
    (if (null? lst)
        (list x)
        (if (pred x (car lst))
            (cons x lst)
            (cons (car lst) (add-sorted x (cdr lst) pred))))))

;; 1.30 Function that sorts based on the given predicate
(define sort/predicate
  (lambda (pred lst)
    (sort-result lst '() pred)))


            

