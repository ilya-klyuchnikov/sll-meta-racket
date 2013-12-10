#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;
;;        AST        ;;
;;;;;;;;;;;;;;;;;;;;;;;

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

; unfold of f-function
(struct unfold () #:transparent)
; constructor match
(struct ctr-match (cname) #:transparent)

; step
(struct step-transient (info expr) #:transparent)
(struct step-stop (expr) #:transparent)
(struct step-decompose (name exprs) #:transparent)
(struct step-variants (variants) #:transparent)

(struct edge-transient (info tree) #:transparent)
(struct edge-decompose (name trees) #:transparent)
(struct edge-variants (variants) #:transparent)

(struct node (expr edge) #:transparent)
(struct leaf (expr) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;
;;    Substitution   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (mk-subst names vals) (map cons names vals))
(define empty-subst '())

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
      [(and (var? e) (assoc (var-name e) subst))
       (cdr (assoc (var-name e) subst))]
      [(var? e) e]
      [else
       (error "// unexpected expr:" e)]))
  (s e))

; applies sub2 to a sub1 (sub1 /// sub2):
; (/// [(n1 . v2) ... (nn . vn)] sub2) =>
;   [(n1 . (// v2 sub2)) .. (nn . (// vn sub2))]
(define (/// sub1 sub2)
  (map (λ (bind) (cons (car bind) (// (cdr bind) sub2))) sub1))

(define (map-values f subst)
  (map (λ (kv) (cons (car kv) (f (cdr kv)))) subst))

;;;;;;;;;;;;;;;;;;;;;;;
;;     Utilities     ;;
;;;;;;;;;;;;;;;;;;;;;;;

; applies a function f to all next expr
; (map-result f (step-transient info e)) =>
;   (step-transient info (f e))
; (map-result f (step-variants ((b1 . e1) (b2 . e2) ... (bn . en) ))) =>
;   (step-variants ((b1 . (f e1)) (b2 . (f e2)) ... (bn . (f en)) ))
(define (map-result f step)
  (cond
    [(step-transient? step)
     (step-transient
      (step-transient-info step)
      (f (step-transient-expr step)))]
    [(step-variants? step)
     (step-variants
      (map (λ (variant) (cons (car variant) (f (cdr variant))))
           (step-variants-variants step)))]
    [else (error "map-result: cannot map " step)]))

; get a (first) f-def by a name
(define
  (program-fdef prog f-name)
  (first (filter (λ (f) (and (fdef? f) (equal? f-name (fdef-name f))))
                 (program-defs prog))))
; get all g-defs by a function name
(define
  (program-gdefs prog name)
  (filter (λ (g) (and (gdef? g) (equal? name (gdef-name g))))
          (program-defs prog)))
; get a g-def by a name and pat
(define
  (program-gdef prog g-name ctr-name)
  (first (filter (λ (g) (equal? ctr-name (pat-name (gdef-pat g))))
                 (program-gdefs prog g-name))))

; create n new vars (adding new suffixes)
; (mk-vars 'x 3) => 'x.1 'x.2 'x.3
(define (mk-vars name n)
  ; (mk-var 'a 1) => (var 'a.1)
  (define ((mk-var name) index)
    (var (string->symbol (string-append
                          (symbol->string name)
                          "."
                          (number->string (+ 1 index))))))
  (map (mk-var name) (range n)))

; enumerates all (unique) variables in an expression
; (vnames (g-call 'x (Nil) 'z 'x)) => '(x z)
(define (vnames e)
  (define (vn e)
    (cond
      [(var? e)
       (list (var-name e))]
      [(atom? e) '()]
      [(ctr? e) (apply append (map vn (ctr-args e)))]
      [(fcall? e) (apply append (map vn (fcall-args e)))]
      [(gcall? e) (apply append (map vn (gcall-args e)))]))
  (remove-duplicates (vn e)))

; maps all variables of an expression to itself
; (id-subst (g-call 'x (Nil) 'z 'x)) => (('x . (var 'x)) ('z (var 'z)))
(define (id-subst e)
  (map (λ (n) (cons n (var n))) (vnames e)))
