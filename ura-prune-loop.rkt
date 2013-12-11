#lang lazy

(require "data.rkt" "process-tree.rkt")
(require lazy/force)
(provide run-ura-prune-loop* run-ura-prune-loop)

; utilities
(struct h-elem (id expr) #:transparent)
(struct q-elem (subst tree history) #:transparent)

(define (tree-id tree)
  (cond
    [(process-leaf? tree) (process-leaf-id tree)]
    [(process-node? tree) (process-node-id tree)]
    [else (error "tree-id unknown tree" tree)]))

(define (tree-expr tree)
  (cond
    [(process-leaf? tree) (process-leaf-expr tree)]
    [(process-node? tree) (process-node-expr tree)]
    [else (error "tree-expr unknown tree" tree)]))

; is current queue-elem is just a dead cycle?
; see notes for details
(define (dead-cycle? elem answer-ids pending-ids)
  (let* ([tree (q-elem-tree elem)]
         [history (q-elem-history elem)]
         [expr (tree-expr tree)]
         [renamings (filter (λ (h-e) (renaming expr (h-elem-expr h-e))) history)]
         [ids (reverse (map h-elem-id renamings))])
    (if (< (length ids) 2) #f
        (let* ([id1 (first ids)]
               [id2 (second ids)]
               [contra-ids1
                (filter (λ (id) (prefix? id1 id)) answer-ids)]
               [contra-ids2
                (filter (λ (id) (and (prefix? id1 id) (not (prefix? id2 id)))) pending-ids)])
          (and (empty? contra-ids1) (empty? contra-ids2))))))


; this is an improved version of URA
; improvement is to detect loops in process tree that produce no answers
; see notes/ for details

(define (ura prog in out)
  (define tree (build-process-tree prog in))
  (define (traverse queue answer-ids)
    (if (empty? queue) '()
        (let* ([qelem (first queue)]
               [queue (rest queue)]
               [subst (q-elem-subst qelem)]
               [tree  (q-elem-tree qelem)]
               [expr (tree-expr tree)]
               [id (tree-id tree)]
               [history  (q-elem-history qelem)]
               [history (cons (h-elem id expr) history)]
               [pending-ids (map (λ (qe) (tree-id (q-elem-tree qelem))) queue)])
          (cond
            [(dead-cycle? qelem answer-ids pending-ids) '()]
            [(and (process-leaf? tree) (equal? out expr))
             (cons subst (traverse queue (cons id answer-ids)))]
            [(process-leaf? tree)
             (traverse queue answer-ids)]
            [(process-node? tree)
             (let ([edge (process-node-edge tree)])
               (cond
                 [(process-edge-transient? edge)
                  (let ([delta
                         (list (q-elem subst (process-edge-transient-tree edge) history))])
                    (traverse (append queue delta) answer-ids))]
                 [(process-edge-variants? edge)
                  (let ([delta
                         (map (λ (v) (q-elem (/// subst (car v)) (cdr v) history))
                              (process-edge-variants-variants edge))])
                    (traverse (append queue delta) answer-ids))]
                 [(process-edge-decompose? edge)
                  (traverse queue answer-ids)]
                 [else (error "ura/traverse unknown edge" edge)]))]
            [else (error "ura/traverse unknown node" tree)]))))
  (traverse (list (q-elem (id-subst in) tree '())) '()))

; strict version of URA - forces entire stream.
; if stream is infinite, this function will not terminate.
(define (run-ura-prune-loop* prog in out)
  (!! (ura prog in out)))

; run URA and returns/forces first n answers.
(define (run-ura-prune-loop n prog in out)
  (!! (take n (ura prog in out))))
