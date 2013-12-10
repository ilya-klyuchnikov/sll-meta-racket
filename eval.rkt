#lang racket

(require "data.rkt")
(provide (all-defined-out))

; stepper performs an elementary step of evaluation
(define (eval-stepper prog)
  (define (step expr)
    (cond
      [(atom? expr)
       (step-stop expr)]
      [(and (ctr? expr) (empty? (ctr-args expr)))
       (step-stop expr)]
      [(ctr? expr)
       (step-decompose (ctr-name expr) (ctr-args expr))]
      [(fcall? expr)
       (let ([f (program-fdef prog (fcall-name expr))])
         (step-transient
          (unfold)
          (// (fdef-body f)
              (mk-subst (fdef-args f)
                   (fcall-args expr)))))]
      [(and (gcall? expr) (ctr? (first (gcall-args expr))))
       (let* ([args (gcall-args expr)]
              [c (first args)]
              [g-name (gcall-name expr)]
              [c-name (ctr-name c)]
              [g-args (rest args)]
              [c-args (ctr-args c)]
              [g-def (program-gdef prog g-name c-name)]
              [g-vs (gdef-args g-def)]
              [g-pat (gdef-pat g-def)]
              [p-vs (pat-vars g-pat)]
              [g-body (gdef-body g-def)]
              [subst (mk-subst (append p-vs g-vs) (append c-args g-args))])
         (step-transient
          (ctr-match c-name)
          (// g-body subst)))]
      [(gcall? expr)
       (let* ([arg (first (gcall-args expr))]
              [args (rest (gcall-args expr))]
              [inner-step (step arg)]
              [expr1 (step-transient-expr inner-step)]
              [info (step-transient-info inner-step)])
         (step-transient info (gcall (cons expr1 args))))]
      [else (error "eval-stepper/step incorrect expression found:" expr)]))
  step)

(define (build-eval-tree prog expr)
  (define stepper (eval-stepper prog))
  (define (aux expr)
    (let ([step (stepper expr)])
      (cond
        [(step-stop? step)
         (leaf (step-stop-expr step))]
        [(step-transient? step)
         (node expr (edge-transient
                     (step-transient-info step)
                     (aux (step-transient-expr step))))]
        [(step-decompose? step)
         (node expr (edge-decompose
                     (step-decompose-name step)
                     (map aux (step-decompose-exprs step))))]
        [else (error "build-eval-tree/aux unknown step:" step)])))
  (aux expr))

(define (eval-tree tree)
  (cond
    [(leaf? tree)
     (leaf-expr tree)]
    [(node? tree)
     (let ([edge (node-edge tree)])
       (cond
         [(edge-transient? edge)
          (eval-tree (edge-transient-tree edge))]
         [(edge-decompose? edge)
          (ctr (edge-decompose-name edge)
               (map eval-tree (edge-decompose-trees edge)))]
         [else (error "eval-tree unknown edge:" edge)]))]))
