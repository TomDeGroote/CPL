#lang eopl
(require rackunit)
(require pict)

;; Define Data structure for expressions
(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;; Use Pattern matching with the above defined data structure to create a function that checks if a variable occurs free in an expression
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and
                   (not (eqv? search-var bound-var))
                   (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or
                (occurs-free? search-var rator)
                (occurs-free? search-var rand))))))

;; Test occurs free
;; If you run your code and this does not fail, then occurs-free? is working
(define t1 (var-exp 'x))
(check-equal? (occurs-free? 'x t1) #t)
;; More tests required, but this was just to see if it worked like expected.

;; 2.24 Given a tree datatype, make a function that transforms a given tree to list
;; The datatype
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

;; The function to extract a bintree to a list
(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) (list 'leaf-node num))
      (interior-node (key left right)
                     (list key (bintree-to-list left) (bintree-to-list right))))))

;; 2.25 use the datastructure from 2.24 to define a function that returns the interior node with the maximum leaf sum
(define max-interior
  (lambda (tree)
    (car (max-interior-with-sum tree))))

;; Find the largest branch in a tree, not really efficient :/
(define max-interior-with-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) '())
      (interior-node (key left right)
                     (if (and (leaf-node? left) (leaf-node? right))
                         (list key (tree-sum tree))
                         (if (leaf-node? left)
                             (if (> (tree-sum tree) (cadr (max-interior-with-sum right)))
                                 (list key (tree-sum tree))
                                 (max-interior-with-sum right))
                             (if (> (tree-sum tree) (cadr (max-interior-with-sum left)))
                                 (list key (tree-sum tree))
                                 (max-interior-with-sum left))))))))
                                 
                             
                             
                                       
    

;; Function to check if leafnode
(define leaf-node?
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) #t)
      (interior-node (key left right) #f))))
;; Function to calculate the sum of a interior-node
(define tree-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) num)
      (interior-node (key left right)
                     (+ (tree-sum left) (tree-sum right))))))

;; Test 2.25
(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 2)))

(check-equal? (tree-sum tree-1) 5)
(check-equal? (tree-sum tree-2) 4)
(check-equal? (tree-sum tree-3) 6)

(check-equal? (max-interior tree-1) 'foo)
(check-equal? (max-interior tree-2) 'foo)
(check-equal? (max-interior tree-3) 'baz)