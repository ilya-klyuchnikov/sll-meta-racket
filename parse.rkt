#lang racket

(require "data.rkt")
(provide (all-defined-out))

;; provides functions to convert from/to s-expressions to/from SLL AST

; in the following we assume that a program is in correct syntax form
(define (parse-program s-prog)
  (program (map parse-def s-prog)))

(define (parse-def s-def)
  (let* ([lhs (first s-def)]
         [rhs (third s-def)]
         [body (parse-expr rhs)]
         [name (first lhs)]
         [params (rest lhs)]
         [f/g (string-ref (symbol->string name) 0)])
    (cond
      [(equal? f/g #\f)
       (fdef name params body)]
      [(equal? f/g #\g)
       (gdef name (parse-pat (first params)) (rest params) body)])))

(define (parse-pat s-pat)
  (pat (first s-pat) (rest s-pat)))

(define (parse-expr s-expr)
  (cond [(symbol? s-expr) (var s-expr)]
        [(and (equal? 'quote (first s-expr)) (symbol? (second s-expr)))
         (atom (second s-expr))]
        [else (let* ([rator (first s-expr)]
                     [s-rands (rest s-expr)]
                     [args (map parse-expr s-rands)]
                     [f/g/c (string-ref (symbol->string rator) 0)])
                (cond
                  [(equal? #\f f/g/c) (fcall rator args)]
                  [(equal? #\g f/g/c) (gcall rator args)]
                  [else (ctr rator args)]))]))

(define (unparse-expr expr)
  (cond
    [(var? expr)
     (var-name expr)]
    [(atom? expr)
     (list 'quote (atom-a expr))]
    [(fcall? expr)
     (cons (fcall-name expr) (map unparse-expr (fcall-args expr)))]
    [(gcall? expr)
     (cons (gcall-name expr) (map unparse-expr (gcall-args expr)))]
    [(ctr? expr)
     (cons (ctr-name expr) (map unparse-expr (ctr-args expr)))]))
