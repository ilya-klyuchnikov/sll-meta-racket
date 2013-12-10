#lang racket

(require "data.rkt" "parse.rkt" "eval.rkt" rackunit)

(define s-prog
  '[{(f-main xs ys) = (g-append xs ys)}
    {(g-append (Nil) ys) = ys}
    {(g-append (Cons x xs) ys) = (Cons x (g-append xs ys))}])

(define prog
  (parse-program s-prog))

(define (s-eval-tree s-expr)
  (build-eval-tree prog (parse-expr s-expr)))

(define (s-eval s-expr)
  (unparse-expr (eval-tree (s-eval-tree s-expr))))

(check-equal?
 (s-eval '(Nil)) '(Nil))

(check-exn
 exn:fail?
 (λ () (s-eval 'a))
 "only ground terms may be evaluated")

(check-equal?
 (s-eval '(Cons 'a (Nil))) '(Cons 'a (Nil)))

(check-equal?
 (s-eval
  '(g-append (Nil) (Nil)))
 '(Nil))

(check-equal?
 (s-eval '(g-append (Cons 'a (Cons 'b (Nil))) (Cons 'c (Cons 'd (Nil)))))
 '(Cons 'a (Cons 'b (Cons 'c (Cons 'd (Nil))))))
