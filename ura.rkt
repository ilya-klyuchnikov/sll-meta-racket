#lang racket

(require "data.rkt" "process-tree.rkt")
(provide ura)

(define (ura prog in out)
  (define tree (build-process-tree prog in))
  (define (traverse queue)
    (if (empty? queue) '()
        (let* ([el (car queue)]
               [sub (car el)]
               [t (cdr el)]
               [q1 (rest queue)])
          (cond
            [(and (process-leaf? t) (equal? out (process-leaf-expr t)))
             (cons sub (traverse q1))]
            [(process-leaf? t)
             (traverse q1)]
            [(process-node? t)
             (let ([edge (process-node-edge t)])
               (cond
                 [(process-edge-transient? edge)
                  (traverse (append q1
                                    (list (cons sub (process-edge-transient-tree edge)))))]
                 [(process-edge-variants? edge)
                  (let ([q2 (map (λ (v) (cons (/// sub (car v)) (cdr v)))
                                 (process-edge-variants-variants edge))])
                    (traverse (append q1 q2)))]
                 [else (error "ura/traverse unknown edge" edge)]))]
            [else (error "ura/traverse unknown node" t)]))))
  (traverse (list (cons (id-subst in) tree))))