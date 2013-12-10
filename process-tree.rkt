#lang racket

(require "data.rkt" "meta-step.rkt")
(provide (all-defined-out))

(struct process-edge-transient (info tree) #:transparent)
(struct process-edge-decompose (name trees) #:transparent)
(struct process-edge-variants (variants) #:transparent)

(struct process-node (expr edge) #:transparent)
(struct process-leaf (expr) #:transparent)

; the first attempt of process tree
; next step: make it lazy
(define (build-process-tree prog expr)
  (define stepper (perfect-meta-stepper prog))
  (define (aux expr)
    (let ([step (stepper expr)])
      (cond
        [(step-stop? step)
         (process-leaf (step-stop-expr step))]
        [(step-transient? step)
         (process-node expr (process-edge-transient
                             (step-transient-info step)
                             (aux (step-transient-expr step))))]
        [(step-variants? step)
         (process-node expr (process-edge-variants
                             (map (λ (v)
                                    (cons (car v) (aux (cdr v))))
                                  (step-variants-variants step))))]
        [(step-decompose? step)
         (process-node expr (process-edge-decompose
                             (step-decompose-name step)
                             (map aux (step-decompose-exprs step))))]
        [else (error "build-eval-tree/aux unknown step:" step)])))
  (aux expr))