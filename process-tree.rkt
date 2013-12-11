#lang lazy

(require "data.rkt" "meta-step.rkt")
(provide (all-defined-out))

; Process tree is potentially infinite,
; but we need only a finite part of it.
; So, we use lazy racket to build process tree.

(struct process-edge-transient (info tree) #:transparent)
(struct process-edge-decompose (name trees) #:transparent)
(struct process-edge-variants (variants) #:transparent)

(struct process-node (id expr edge) #:transparent)
(struct process-leaf (id expr) #:transparent)

(define (build-process-tree prog expr)
  (define stepper (perfect-meta-stepper prog))
  (define (build expr id)
    (let ([step (stepper expr)])
      (cond
        [(step-stop? step)
         (process-leaf id (step-stop-expr step))]
        [(step-transient? step)
         (process-node id expr (process-edge-transient
                                (step-transient-info step)
                                (build (step-transient-expr step) (cons 0 id))))]
        [(step-variants? step)
         (process-node id expr (process-edge-variants
                                (map-with-index (λ (v i) (cons (car v) (build (cdr v) (cons i id))))
                                                (step-variants-variants step))))]
        [(step-decompose? step)
         (process-node id expr (process-edge-decompose
                                (step-decompose-name step)
                                (map-with-index (λ (e i) (build e (cons i id))) 
                                                (step-decompose-exprs step))))]
        [else (error "build-eval-tree/build unknown step:" step)])))
  (build expr '()))
