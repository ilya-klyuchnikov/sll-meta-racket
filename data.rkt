#lang racket

(provide (all-defined-out))

; expressions
(struct var (name) #:transparent)
(struct atom (a) #:transparent)
(struct ctr (name args) #:transparent)
(struct fcall (name args) #:transparent)
(struct gcall (name args) #:transparent)

(struct pat (name vars) #:transparent)
(struct fdef (name args body) #:transparent)
(struct gdef (name pat args body) #:transparent)
(struct program (defs) #:transparent)

;; info for transient step
;; unfold of f-function
(struct unfold () #:transparent)
;; constructor match
(struct ctr-match (cname) #:transparent)

;; step
(struct step-transient (info expr) #:transparent)
(struct step-stop (expr) #:transparent)
(struct step-decompose (name exprs) #:transparent)

(struct edge-transient (info tree) #:transparent)
(struct edge-decompose (name trees) #:transparent)

(struct node (expr edge) #:transparent)
(struct leaf (expr) #:transparent)

; Utilities
; get a (first) f-def by a name
(define
  (program-fdef prog f-name)
  (first (filter (λ (f) (and (fdef? f) (equal? f-name (fdef-name f))))
                 (program-defs prog))))
; get a g-def by a aname + pattern
(define
  (program-gdef prog g-name ctr-name)
  (first (filter (λ (g) (and (gdef? g)
                             (equal? g-name (gdef-name g))
                             (equal? ctr-name (pat-name (gdef-pat g)))))
                 (program-defs prog))))

; get all g-defs by a function name
(define
  (program-gdefs prog name)
  (filter (λ (g) (and (gdef? g) (equal? name (gdef-name g))))))

; lookup a rhs of a binding
(define lookup
  assoc)

(define (mk-subst l1 l2)
  (map cons l1 l2))

; apply a substitution to an expression
(define
  (// e subst)
  (define (s e)
    (cond
      [(atom? e)
       e]
      [(ctr? e)
       (ctr (ctr-name e) (map s (ctr-args e)))]
      [(fcall? e)
       (fcall (fcall-name e) (map s (fcall-args e)))]
      [(gcall? e)
       (gcall (gcall-name e) (map s (gcall-args e)))]
      [(and (var? e) (lookup (var-name e) subst))
       (cdr (lookup (var-name e) subst))]
      [(var? e) e]
      [else 
       (error "// unexpected expr:" e)]))
  (s e))
