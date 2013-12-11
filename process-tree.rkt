#lang lazy

(require "data.rkt" "meta-step.rkt")
(provide (all-defined-out))

; Process tree is potentially infinite,
; but we need only a finite part of it.
; So, we use lazy racket to build process tree.

(struct process-edge-transient (info tree) #:transparent)
(struct process-edge-decompose (name trees) #:transparent)
(struct process-edge-variants (variants) #:transparent)

(struct process-node (expr edge) #:transparent)
(struct process-leaf (expr) #:transparent)

(define (build-process-tree prog expr)
  (define stepper (perfect-meta-stepper prog))
  (define (build expr)
    (let ([step (stepper expr)])
      (cond
        [(step-stop? step)
         (process-leaf (step-stop-expr step))]
        [(step-transient? step)
         (process-node expr (process-edge-transient
                             (step-transient-info step)
                             (build (step-transient-expr step))))]
        [(step-variants? step)
         (process-node expr (process-edge-variants
                             (map (λ (v) (cons (car v) (build (cdr v))))
                                  (step-variants-variants step))))]
        [(step-decompose? step)
         (process-node expr (process-edge-decompose
                             (step-decompose-name step)
                             (map build (step-decompose-exprs step))))]
        [else (error "build-eval-tree/build unknown step:" step)])))
  (build expr))
