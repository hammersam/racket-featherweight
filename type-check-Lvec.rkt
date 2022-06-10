#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Lwhile.rkt")
(provide type-check-Lvec type-check-Lvec-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Tuples (aka Vectors)                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lvec

(define type-check-Lvec-class
  (class type-check-Lwhile-class
    (super-new)
    (inherit check-type-equal?)

    ;; (vector (vector 1) 2 3)
    ;; (vector 1) -- (HasType (Prim 'vector (list 1)) (Vector 'Integer))
    ;; 2 -- (Int 2)
    ;; 3 -- (Int 3)

    (define/override (type-check-exp env)
      (lambda (e)
        (define recur (type-check-exp env))
        (match e
          [(Prim 'vector es)
           (unless (<= (length es) 50)
             (error 'type-check "vector too large ~a, max is 50" (length es)))
           (define-values (e* t*) (for/lists (e* t*) ([e es]) (recur e)))
           (define t `(Vector ,@t*))
           ;; distinguish elements in tuple that are
           ;; themselves tuples from elements that are not.
           (values (HasType (Prim 'vector e*) t)  t)]
          ;; open backdoor for (vector-ref [vec : (Vector Integer Integer ...)]
          ;;                               [var : Integer])
          [(Prim 'vector-ref (list e1 (Var x)))
           (define-values (e1^ t) (recur e1))
           (define-values (_ t^) (recur (Var x)))
           (match `(,t . ,t^)
             [`((Vector ,ts ...) . Integer)
              (values (Prim 'vector-ref (list e1^ (Var x))) 'Integer)]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t e)])]
          [(Prim 'vector-ref (list e1 (Int i)))
           (define-values (e1^ t) (recur e1))
           (match t
             [`(Vector ,ts ...)
              (unless (and (0 . <= . i) (i . < . (length ts)))
                (error 'type-check "index ~a out of bounds\nin ~v" i e))
              (values (Prim 'vector-ref (list e1^ (Int i)))  (list-ref ts i))]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t e)])]
          [(Prim 'vector-set! (list e1 (Int i) arg) )
           (define-values (e-vec t-vec) (recur e1))
           (define-values (e-arg^ t-arg) (recur arg))
           (match t-vec
             [`(Vector ,ts ...)
              (unless (and (0 . <= . i) (i . < . (length ts)))
                (error 'type-check "index ~a out of bounds\nin ~v" i e))
              ;; Does the type of arg need to be same as
              ;; the element in the tuple being replaced?
              (check-type-equal? (list-ref ts i) t-arg e)
              (values (Prim 'vector-set! (list e-vec (Int i) e-arg^))  'Void)]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t-vec e)])]
          [(Prim 'vector-length (list e))
           (define-values (e^ t) (recur e))
           (match t
             [`(Vector ,ts ...)
              (values (Prim 'vector-length (list e^))  'Integer)]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t e)])]
          [(Prim 'eq? (list arg1 arg2))
           (define-values (e1 t1) (recur arg1))
           (define-values (e2 t2) (recur arg2))
           (match* (t1 t2)
             [(`(Vector ,ts1 ...)  `(Vector ,ts2 ...))  (void)]
             [(other wise)  (check-type-equal? t1 t2 e)])
           (values (Prim 'eq? (list e1 e2)) 'Boolean)]
          [(HasType (Prim 'vector es) t)
           ((type-check-exp env) (Prim 'vector es))]
          [(HasType e1 t)
           (define-values (e1^ t^) (recur e1))
           (check-type-equal? t t^ e)
           (values (HasType e1^ t) t)]
          [(GlobalValue name)
           (values (GlobalValue name) 'Integer)]
          [(Allocate size t)
           (values (Allocate size t) t)]
          [(Collect size)
           (values (Collect size) 'Void)]
          [else ((super type-check-exp env) e)]
          )))
    ))

(define (type-check-Lvec p)
  (send (new type-check-Lvec-class) type-check-program p))

#;(define (type-check-exp env)
  (send (new type-check-Lvec-class) type-check-exp env))

#;(define (type-equal? t1 t2)
  (send (new type-check-Lvec-class) type-equal? t1 t2))

