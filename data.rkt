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

; renaming is a map from names to names
; returns either #f (if no renaming) or renaming

; returns a list of optional elementary renamings
(define (ren e1 e2)
  (cond
    [(and (var? e1) (var? e2))
     (list (cons (var-name e1) (var-name e2)))]
    [(and (ctr? e1) (ctr? e2) (equal? (ctr-name e1) (ctr-name e2)))
     (apply append (map ren (ctr-args e1) (ctr-args e2)))]
    [(and (fcall? e1) (fcall? e2) (equal? (fcall-name e1) (fcall-name e2)))
     (apply append (map ren (fcall-args e1) (fcall-args e2)))]
    [(and (gcall? e1) (gcall? e2) (equal? (gcall-name e1) (gcall-name e2)))
     (apply append (map ren (gcall-args e1) (gcall-args e2)))]
    [else (list #f)]))

(define (zip l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [else (cons (cons (first l1) (first l2))
                (zip (rest l1) (rest l2)))]))

; Here is a simple trick.
; If there is a renaming, then it will be
; (zip (vnames e1) (vnames e2))
(define (renaming e1 e2)
  (let ([res (remove-duplicates (ren e1 e2))]
        [expected-ren (zip (vnames e1) (vnames e2))])
        (and (equal? res expected-ren) expected-ren)))

(define (prefix? path1 path2)
  (cond
    [(> (length path1) (length path2))
     #f]
    [(= (length path1) (length path2))
     (equal? path1 path2)]
    [else
     (prefix? path1 (rest path2))]))

(define (map-with-index f xs)
  (map f xs (range (length xs))))
