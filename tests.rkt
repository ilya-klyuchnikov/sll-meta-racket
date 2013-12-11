#lang racket

(require "data.rkt"
         "parse.rkt"
         "eval.rkt"
         "meta-step.rkt"
         "process-tree.rkt"
         "ura.rkt"
         "ura-prune-loop.rkt"
         rackunit)

(define (s-renaming se1 se2)
  (renaming (parse-expr se1) (parse-expr se2)))

; all tests are done wrt this tiny program
(define s-prog
  '[
    {(f-main xs ys) = (g-append xs ys)}
    {(g-flatten (Leaf x)) = (Cons x (Nil))}
    {(g-flatten (Node lt x rt)) = (g-append (g-flatten lt) (Cons x (g-flatten rt)) )}
    ; list concatenation
    {(g-append (Nil) ys) = ys}
    {(g-append (Cons x xs) ys) = (Cons x (g-append xs ys))}
    ; equality over char (A|B)
    {(g-eq (A) s) = (g-eq-A s)}
    {(g-eq (B) s) = (g-eq-B s)}
    {(g-eq-A (A)) = (T)}
    {(g-eq-A (B)) = (F)}
    {(g-eq-B (A)) = (F)}
    {(g-eq-B (B)) = (T)}
    ; equality over 2 lists
    {(g-eq-list (Nil) l2) = (g-eq-list-nil l2)}
    {(g-eq-list (Cons x xs) l2) = (g-eq-list-cons l2 x xs)}
    {(g-eq-list-cons (Nil) x xs) = (F)}
    {(g-eq-list-cons (Cons y ys) x xs) = (g-&& (g-eq x y) (g-eq-list xs ys))}
    {(g-eq-list-nil (Nil)) = (T)}
    {(g-eq-list-nil (Cons x xs)) = (F)}
    ; boolean and (short-circuit and)
    {(g-&& (F) b) = (F)}
    {(g-&& (T) b) = b}
    ; total &
    {(g-& (F) b) = (g-b b (F))}
    {(g-& (T) b) = (g-b b b)}
    ; dummy function - just to enforce pattern matching
    {(g-b (F) x) = x}
    {(g-b (T) x) = x}
    ; idle function for tests
    {(g-zero (Zero) x) = x}
    {(g-zero (Succ n) x) = (g-zero n (F))}
    ])

(define prog
  (parse-program s-prog))

(define (s-eval-tree s-expr)
  (build-eval-tree prog (parse-expr s-expr)))

(define (s-eval s-expr)
  (unparse-expr (eval-tree (s-eval-tree s-expr))))

(define (s-ura* s-in s-out)
  (let ([res (run-ura* prog (parse-expr s-in) (parse-expr s-out))])
    (map (λ (sub) (map-values unparse-expr sub)) res)))

(define (s-ura1* s-in s-out)
  (let ([res (run-ura-prune-loop* prog (parse-expr s-in) (parse-expr s-out))])
    (map (λ (sub) (map-values unparse-expr sub)) res)))

(define (s-ura n s-in s-out)
  (let ([res (run-ura n prog (parse-expr s-in) (parse-expr s-out))])
    (map (λ (sub) (map-values unparse-expr sub)) res)))

(define (s-ura1 n s-in s-out)
  (let ([res (run-ura-prune-loop n prog (parse-expr s-in) (parse-expr s-out))])
    (map (λ (sub) (map-values unparse-expr sub)) res)))

;;;;;;;;;;;;;;;;;;;;
;;   Path         ;;
;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (prefix? '() '())
 #t)

(check-equal?
 (prefix? '(2 1) '(3 2 1))
 #t)

(check-equal?
 (prefix? '(3 2 1) '(2 1))
 #f)

;;;;;;;;;;;;;;;;;;;;
;;   Renaming     ;;
;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (s-renaming '(A) '(A))
 '())

(check-equal?
 (s-renaming '(A) '(B))
 #f)

(check-equal?
 (s-renaming '(P x y) '(P y x))
 '((x . y) (y . x)))

(check-equal?
 (s-renaming '(P x x) '(P y x))
 #f)

;;;;;;;;;;;;;;;;;;;;
;;   Eval tests   ;;
;;;;;;;;;;;;;;;;;;;;

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
 (s-eval
  '(g-append (g-append (Nil) (Nil)) (Nil)))
 '(Nil))

(check-equal?
 (s-eval '(g-append (Cons 'a (Cons 'b (Nil))) (Cons 'c (Cons 'd (Nil)))))
 '(Cons 'a (Cons 'b (Cons 'c (Cons 'd (Nil))))))

;;;;;;;;;;;;;;;;;;;;;
;;       URA       ;;
;;;;;;;;;;;;;;;;;;;;;

; no solution
(check-equal?
 (s-ura* '(g-eq (A) (A)) '(F))
 (list))

(check-equal?
 (s-ura1* '(g-eq (A) (A)) '(F))
 (list))

; one solution - empty subst
(check-equal?
 (s-ura* '(g-eq (A) (A)) '(T))
 '{
   []
   })

(check-equal?
 (s-ura1* '(g-eq (A) (A)) '(T))
 '{
   []
   })

(check-equal?
 (s-ura* '(g-eq x (A)) '(T))
 '{
   [(x . (A))]
   })

(check-equal?
 (s-ura1* '(g-eq x (A)) '(T))
 '{
   [(x . (A))]
   })

(check-equal?
 (s-ura* '(g-eq (A) x) '(T))
 '{
   [(x . (A))]
   })

(check-equal?
 (s-ura1* '(g-eq (A) x) '(T))
 '{
   [(x . (A))]
   })

(check-equal?
 (s-ura* '(g-eq x x) '(T))
 '{
   [(x . (A))]
   [(x . (B))]
   })

(check-equal?
 (s-ura1* '(g-eq x x) '(T))
 '{
   [(x . (A))]
   [(x . (B))]
   })

(check-equal?
 (s-ura* '(g-eq x x) '(F))
 '{
   })

(check-equal?
 (s-ura1* '(g-eq x x) '(F))
 '{
   })

(check-equal?
 (s-ura* '(g-eq x y) '(T))
 '{
   [(x . (A))  (y . (A))]
   [(x . (B))  (y . (B))]
   })

(check-equal?
 (s-ura1* '(g-eq x y) '(T))
 '{
   [(x . (A))  (y . (A))]
   [(x . (B))  (y . (B))]
   })

(check-equal?
 (s-ura* '(g-eq x y) '(F))
 '{
   [(x . (A))  (y . (B))]
   [(x . (B))  (y . (A))]
   })

(check-equal?
 (s-ura* '(g-&& (g-eq x y) (g-eq x z)) '(T))
 '{
   [(x . (A))  (y . (A))  (z . (A))]
   [(x . (B))  (y . (B))  (z . (B))]
   })

(check-equal?
 (s-ura1* '(g-&& (g-eq x y) (g-eq x z)) '(T))
 '{
   [(x . (A))  (y . (A))  (z . (A))]
   [(x . (B))  (y . (B))  (z . (B))]
   })

; this shows some asymmetry of URA wrt relations
; when relation function doesn't consider all cases (using else, otherwise, ...)
(check-equal?
 (s-ura* '(g-&& (g-eq x y) (g-eq x z)) '(F))
 '{
   [(x . (A))  (y . (B))  (z . z)]
   [(x . (B))  (y . (A))  (z . z)]
   [(x . (A))  (y . (A))  (z . (B))]
   [(x . (B))  (y . (B))  (z . (A))]
   })

(check-equal?
 (s-ura1* '(g-&& (g-eq x y) (g-eq x z)) '(F))
 '{
   [(x . (A))  (y . (B))  (z . z)]
   [(x . (B))  (y . (A))  (z . z)]
   [(x . (A))  (y . (A))  (z . (B))]
   [(x . (B))  (y . (B))  (z . (A))]
   })

; this answer is more detailed since "testing" function & is more detailed
(check-equal?
 (s-ura* '(g-& (g-eq x y) (g-eq x z)) '(F))
 '{
   [(x . (A))  (y . (B))  (z . (A))]
   [(x . (A))  (y . (B))  (z . (B))]
   [(x . (B))  (y . (A))  (z . (A))]
   [(x . (B))  (y . (A))  (z . (B))]
   [(x . (A))  (y . (A))  (z . (B))]
   [(x . (B))  (y . (B))  (z . (A))]
   })

(check-equal?
 (s-ura1* '(g-& (g-eq x y) (g-eq x z)) '(F))
 '{
   [(x . (A))  (y . (B))  (z . (A))]
   [(x . (A))  (y . (B))  (z . (B))]
   [(x . (B))  (y . (A))  (z . (A))]
   [(x . (B))  (y . (A))  (z . (B))]
   [(x . (A))  (y . (A))  (z . (B))]
   [(x . (B))  (y . (B))  (z . (A))]
   })

(check-equal?
 (s-ura* '(g-eq-list (g-append x y) (Nil)) '(T))
 '{
   [(x . (Nil))  (y . (Nil))]
   })

(check-equal?
 (s-ura1* '(g-eq-list (g-append x y) (Nil)) '(T))
 '{
   [(x . (Nil))  (y . (Nil))]
   })


(check-equal?
 (s-ura* '(g-eq-list (g-append x y) (Cons (A) (Nil))) '(T))
 '{
   [(x . (Nil))  (y . (Cons (A) (Nil)))]
   [(x . (Cons (A) (Nil)))  (y . (Nil))]
   })

(check-equal?
 (s-ura1* '(g-eq-list (g-append x y) (Cons (A) (Nil))) '(T))
 '{
   [(x . (Nil))  (y . (Cons (A) (Nil)))]
   [(x . (Cons (A) (Nil)))  (y . (Nil))]
   })

; this example shows that the form how relation is coded matters
(check-equal?
 (s-ura* '(g-eq-list (g-append x y) (Nil)) '(F))
 '{
   [(x . (Nil))  (y . (Cons y.1 y.2))]
   [(x . (Cons x.1 x.2))  (y . y)]
   })

(check-equal?
 (s-ura1* '(g-eq-list (g-append x y) (Nil)) '(F))
 '{
   [(x . (Nil))  (y . (Cons y.1 y.2))]
   [(x . (Cons x.1 x.2))  (y . y)]
   })

(require racket/sandbox)

; checks that ura loops for this example
(check-exn
 exn:fail?
 (λ () (with-limits 1 50 (s-ura* '(g-zero x (T)) '(T))))
 "should loop")

; however, we can output the first result
(check-equal?
 (s-ura 1 '(g-zero x (T)) '(T))
 '{
   [(x . (Zero))]
   })

; smart-ura is capable of detecting cycles!!
(check-equal?
 (s-ura1* '(g-zero x (T)) '(T))
 '{
   [(x . (Zero))]
   })

; checks that ura loops for this example
(check-exn
 exn:fail?
 (λ () (with-limits 1 50 (s-ura 1 '(g-eq-list (g-flatten x) (Nil)) '(T))))
 "should loop")

; smart-ura is not capable of detecting cycles (needs a generalization)
(check-exn
 exn:fail?
 (λ () (with-limits 1 50 (s-ura1 1 '(g-flatten x) '(Nil))))
 "should loop")
