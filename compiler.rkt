#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp.rkt")
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "type-check-Lvar.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref env x))]
      [(Int n) (Int n)]
      [(Let x e body)
       (let ([uniquified_x (gensym x)])
         (Let uniquified_x
              ((uniquify-exp env) e)
              ((uniquify-exp (dict-set env x uniquified_x)) body)))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (letrec ([rco_atm
            (lambda (e)
              (match e
                [(Int int) (values e '())]
                [(Var var) (values e '())]
                [(Prim 'read '())
                 (let ([tmp-name (gensym 'tmp)])
                   (values tmp-name
                           (cons (cons tmp-name (Prim 'read '()))
                                 '())))]
                [(Prim '- `(,arg)) ;; Do I need to make sure that arg is atomic? - absolutely
                 (define-values (name env) (rco_atm arg))
                 (let ([tmp-name (gensym 'tmp)]
                       [v (dict-ref env name #f)])
                   (values tmp-name
                           (cons (cons tmp-name
                                       (if v
                                           (Let name v (Prim '- (list (Var name))))
                                           e))
                                 env)))]
                [(Prim suborplus `(,arg1 ,arg2))
                 (define-values (name1 env1) (rco_atm arg1))
                 (define-values (name2 env2) (rco_atm arg2))
                 (let ([tmp-name (gensym 'tmp)]
                       [v1 (dict-ref env1 name1 #f)]
                       [v2 (dict-ref env2 name2 #f)])
                   (values tmp-name
                           (cons (cons tmp-name
                                       (cond [(and v1 v2)
                                              (Let name1
                                                   v1
                                                   (Let name2
                                                        v2
                                                        (Prim suborplus (list (Var name1) (Var name2)))))]
                                             [v1
                                              (Let name1
                                                   v1
                                                   (Prim suborplus (list (Var name1) name2)))]
                                             [v2
                                              (Let name2
                                                   v2
                                                   (Prim suborplus (list name1 (Var name2))))]
                                             [else e]))
                                 (append env1 env2))))]
                [(Let var e1 e2)
                 (let ([tmp-name (gensym 'tmp)])
                   (values tmp-name
                           (cons (cons tmp-name (Let var (rco_exp e1) (rco_exp e2)))
                                 '())))]))]
           [rco_exp
            (lambda (e)
              (match e
                [(Int int) e]
                [(Var var) e]
                [(Prim 'read '()) e]
                [(Prim '- `(,arg))
                 (define-values (name env) (rco_atm e)) ;; (- (- 10)) (let ((tmp (- 10))) (- tmp))
                 (dict-ref env name)]
                [(Prim suborplus `(,arg1 ,arg2))
                 (define-values (name env) (rco_atm e))
                 (dict-ref env name)]
                [(Let var e1 e2) (Let var (rco_exp e1) (rco_exp e2))]))])
           (match p
             [(Program info e) (Program info (rco_exp e))])))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (letrec ([explicate_tail
            (lambda (e)
              (match e
                [(Var x) (Return (Var x))]
                [(Int n) (Return (Int n))]
                [(Let x rhs body)
                 (explicate_assign rhs x (explicate_tail body))]
                [(Prim op es) (Return (Prim op es))]
                [else (error "explicate_tail unhandled case" e)]))]
           [explicate_assign
            (lambda (e x cont)
              (match e
                [(Var x^) (Seq (Assign (Var x) (Var x^)) cont)]
                [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
                [(Let y rhs body)
                 (explicate_assign rhs y (explicate_assign body x cont))]
                [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]
                [else (error "explicate_assign unhandled case" e)]))])
    (match p
      [(Program info body)
       (CProgram info (list (cons 'start (explicate_tail body))))])))


;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (letrec ([tail-handler
            (lambda (tail)
              (match tail
                [(Return e)
                 (append (stmt-handler (Assign (Reg 'rax) e))
                         (list (Jmp 'conclusion)))]
                [(Seq stmt tail)
                 (append (stmt-handler stmt)
                         (tail-handler tail))]))]
           [stmt-handler
            (lambda (stmt)
              (match stmt
                [(Assign v e)
                 (match e
                   [(Prim 'read '())
                    (list (Callq 'read_int 0)
                          (Instr 'movq (list (Reg 'rax) v)))]
                   [(Prim '- `(,atm))
                    (let ([atm^ (atm-handler atm)])
                      (list (Instr 'movq (list atm^ v))
                            (Instr 'negq (list v))))]
                   [(Prim '+ `(,atm1 ,atm2))
                    (let* ([atm1^ (atm-handler atm1)]
                           [atm2^ (atm-handler atm2)])
                      (cond
                        [(equal? atm1^ v)
                         (list (Instr 'addq (list atm2^ v)))]
                        [(equal? atm2^ v)
                         (list (Instr 'addq (list atm1^ v)))]
                        [else
                         (list (Instr 'movq (list atm1^ v))
                               (Instr 'addq (list atm2^ v)))]))]
                   [(Prim '- `(,atm1 ,atm2))
                    (let* ([atm1^ (atm-handler atm1)]
                           [atm2^ (atm-handler atm2)])
                      (cond
                        [(equal? atm1^ v)
                         (list (Instr 'subq (list atm2^ v)))]
                        [else
                         (list (Instr 'movq (list atm1^ v))
                               (Instr 'subq (list atm2^ v)))]))]
                   [(Int int) (list (Instr 'movq (list (atm-handler e) v)))]
                   [(Var var) (list (Instr 'movq (list (atm-handler e) v)))])]))]
           [atm-handler
            (lambda (atm)
              (match atm
                [(Int int) (Imm int)]
                [(Var var) atm]))])
    (match p
      [(CProgram info (list (cons label tail)))
       (X86Program info (list (cons label (Block '() (tail-handler tail)))))])))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (match p
    [(X86Program info (list (cons label (Block '() instructions))))
     (let* ([vars-size 0]
            [vars-locations
             (map (lambda (var-type)
                    (set! vars-size (+ vars-size 8))
                    (cons (Var (car var-type))     ;; make it more convenient to access the stack
                          (Deref 'rbp (- vars-size))))  ;; locations of variables in instructions later.
                  (dict-ref info 'locals-types))]
            [total-size (if (= (modulo vars-size 16) 0) (+ vars-size 16) (+ vars-size 24))]
            [assigned-instrs
             (map (lambda (instr)
                    (match instr
                      [(Instr one-arg `(,arg))
                       (Instr one-arg `(,(dict-ref vars-locations arg arg)))]
                      [(Instr two-args `(,arg1 ,arg2))
                       (Instr two-args
                              `(,(dict-ref vars-locations arg1 arg1)
                                ,(dict-ref vars-locations arg2 arg2)))]
                      [else instr]))
                  instructions)])
       (X86Program
        (cons (cons 'stack-space total-size) info)
        (list (cons label (Block '() assigned-instrs)))))]))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (match p
    [(X86Program info `(,(cons label (Block '() instructions))))
     (let ((patched-instrs
            (foldr (lambda (instr accu)
                     (match instr
                       [(Instr two-args `(,(? Deref? arg1) ,(? Deref? arg2)))
                        (cons (Instr 'movq `(,arg1 ,(Reg 'rax)))
                              (cons (Instr two-args `(,(Reg 'rax) ,arg2))
                                    accu))]
                       [else (cons instr accu)]))
                   '()
                   instructions)))
       (X86Program info (list (cons label (Block '() patched-instrs)))))]))

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info `(,(cons label block)))
     (let ((main
            (cons 'main (Block '() (list (Instr 'pushq (list (Reg 'rbp)))
                                          (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
                                          (Instr 'subq (list (Imm 16) (Reg 'rsp)))
                                          (Jmp 'start)))))
           (conclusion
            (cons 'conclusion (Block '() (list (Instr 'addq (list (Imm 16) (Reg 'rsp)))
                                                (Instr 'popq (list (Reg 'rbp)))
                                                (Retq))))))
       (X86Program info (list main (cons 'start block) conclusion)))]))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `( ("uniquify" ,uniquify ,interp-Lvar ,type-check-Lvar)
     ;; Uncomment the following passes as you finish them.
     ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar ,type-check-Lvar)
     ("explicate control" ,explicate-control ,interp-Cvar ,type-check-Cvar)
     ("instruction selection" ,select-instructions ,interp-pseudo-x86-0)
     ("assign homes" ,assign-homes ,interp-pseudo-x86-0)
     ("patch instructions" ,patch-instructions ,interp-x86-0)
     ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
     ))

