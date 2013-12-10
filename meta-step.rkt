#lang racket

(require "data.rkt" "eval.rkt")
(provide meta-stepper perfect-meta-stepper)

; an extension of eval-stepper (= driving)
; it considers two additional cases:
; * (gcall (var ...) ...)
; * (var ...)
(define (meta-stepper prog)
  (define step (eval-stepper prog))
  (define (meta-step expr)
    (cond
      [(var? expr)
       (step-stop expr)]
      [(and (gcall? expr) (var? (first (gcall-args expr))))
       (step-variants (map (scrutinize (gcall-args expr))
                           (program-gdefs prog (gcall-name expr))))]
      [(and (gcall? expr) (ctr? (first (gcall-args expr))))
       (step expr)]
      [(gcall? expr)
       (let* ([arg (first (gcall-args expr))]
              [args (rest (gcall-args expr))]
              [inner-step (meta-step arg)])
         (map-result (λ (e) (gcall (gcall-name expr) (cons e args))) inner-step))]
      [else (step expr)]))
  meta-step)

(define (perfect-meta-stepper prog)
  (define stepper (meta-stepper prog))
  (define (perfect-step expr)
    (let ([inner-step (stepper expr)])
      (cond
        [(step-variants? inner-step)
         (step-variants
          (map (λ (variant)
                 (cons (car variant) (// (cdr variant) (car variant))))
               (step-variants-variants inner-step)))]
        [else inner-step])))
  perfect-step)

(define ((scrutinize g-args) g-def)
  (let* ([v (var-name (first g-args))]
         [args (rest g-args)]
         [pat (gdef-pat g-def)]
         [params (gdef-args g-def)]
         [body (gdef-body g-def)]
         [ctr-name (pat-name pat)]
         [ctr-params (pat-vars pat)]
         [fresh-vars (mk-vars v (length ctr-params))]
         [sub (mk-subst (append ctr-params params)
                        (append fresh-vars args))])
    (cons
     (list (cons v (ctr ctr-name fresh-vars))) ; subst
     (// body sub))))
