#lang eopl
(require "syntax.rkt")
(require "LET.rkt")

; let x = -1 in x - 4
(define p6
  (a-program (let-exp 'x
                      (minus-exp (const-exp 1))
                      (diff-exp (var-exp 'x) (const-exp 4)))))

; Program to test 3.9
;let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))
(define p7
  (a-program (let-exp 'x
                      (const-exp 4)
                      (cons-exp
                       (var-exp 'x)
                       (cons-exp
                        (cons-exp
                         (diff-exp (var-exp 'x) (const-exp 1))
                         (emptylist-exp))
                        (emptylist-exp))))))
; let x = 4 in cons(x, emptylist)
(define p8
  (a-program (let-exp 'x
                      (const-exp 4)
                      (cons-exp (var-exp 'x) (var-exp 'x)))))

(define (test p)
  (begin
    (display (program->string p))
    (display "\n")
    (display (expval->string (value-of-program p)))
    (display "\n----------------\n")))
