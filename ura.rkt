#lang lazy

(require "data.rkt" "process-tree.rkt")
(provide (all-defined-out))

; In a general case URA generates an infinite stream of answers.
; So we use lazy racket to construct such stream.

(define (ura prog in out)
  (define tree (build-process-tree prog in))
  (define (traverse queue)
    (if (empty? queue) '()
        (let* ([subst (caar queue)]
               [tree (cdar queue)]
               [queue (rest queue)])
          (cond
            [(and (process-leaf? tree) (equal? out (process-leaf-expr tree)))
             (cons subst (traverse queue))]
            [(process-leaf? tree)
             (traverse queue)]
            [(process-node? tree)
             (let ([edge (process-node-edge tree)])
               (cond
                 [(process-edge-transient? edge)
                  (traverse (append queue
                                    (list (cons subst (process-edge-transient-tree edge)))))]
                 [(process-edge-variants? edge)
                  (let ([delta (map (λ (v) (cons (/// subst (car v)) (cdr v)))
                                    (process-edge-variants-variants edge))])
                    (traverse (append queue delta)))]
                 [else (error "ura/traverse unknown edge" edge)]))]
            [else (error "ura/traverse unknown node" tree)]))))
  (traverse (list (cons (id-subst in) tree))))

; strict version of URA - forces entire stream.
; if stream is infinite, this function will not terminate.
(define (run-ura* prog in out)
  (!! (ura prog in out)))

; run URA and returns/forces first n answers.
(define (run-ura n prog in out)
  (!! (take n (ura prog in out))))
