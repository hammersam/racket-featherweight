#lang racket

(require "utilities.rkt")
(require "compiler.rkt")
(require graph)

(define program1
  (X86Program '() `(,(cons 'start (Block '() (list (Instr 'movq (list (Imm 5) (Var 'a)))
                                                   (Instr 'movq (list (Imm 30) (Var 'b)))
                                                   (Instr 'movq (list (Var 'a) (Var 'c)))
                                                   (Instr 'movq (list (Imm 10) (Var 'b)))
                                                   (Instr 'addq (list (Var 'b) (Var 'c)))))))))

(define program2
  (X86Program (list (cons 'locals-types (list (cons (Var 'x) 'int)
                                              (cons (Var 'y) 'int)
                                              (cons (Var 'z) 'int)
                                              (cons (Var 'w) 'int)
                                              (cons (Var 't) 'int)
                                              (cons (Var 'v) 'int))))
              `(,(cons 'start (Block '() (list (Instr 'movq (list (Imm 1) (Var 'v)))
                                               (Instr 'movq (list (Imm 42) (Var 'w)))
                                               (Instr 'movq (list (Var 'v) (Var 'x)))
                                               (Instr 'addq (list (Imm 7) (Var 'x)))
                                               (Instr 'movq (list (Var 'x) (Var 'y)))
                                               (Instr 'movq (list (Var 'x) (Var 'z)))
                                               (Instr 'addq (list (Var 'w) (Var 'z)))
                                               (Instr 'movq (list (Var 'y) (Var 't)))
                                               (Instr 'negq (list (Var 't)))
                                               (Instr 'movq (list (Var 'z) (Reg 'rax)))
                                               (Instr 'addq (list (Var 't) (Reg 'rax)))
                                               (Jmp 'conclusion)))))))

(define patched_prog2
  (patch-instructions program2))

(match patched_prog2
  [(X86Program info `(,(cons label (Block binfo instructions))))
   instructions])


;; (define ((uncover_live_test expected) p)
;;   (match (uncover_live p)
;;     [(X86Program _ `(,(cons _ (Block info _))))
;;      (let ((liafsets (dict-ref info 'live-after-sets)))
;;        (foldl (lambda (s accu)
;;                 (if (= (set-count (set-subtract (car s) (cdr s))) 0)
;;                     (and #t accu)
;;                     #f))
;;               #t
;;               (map cons liafsets expected)))]))

;; (map (lambda (e p) ((uncover_live_test e) p))
;;      (list
;;       (list (set (Var 'a))
;;             (set (Var 'a))
;;             (set (Var 'c))
;;             (set (Var 'c) (Var 'b))
;;             (set))
;;       (list (set (Var 'v) (Reg 'rsp))
;;             (set (Var 'w) (Var 'v) (Reg 'rsp))
;;             (set (Var 'w) (Var 'x) (Reg 'rsp))
;;             (set (Var 'w) (Var 'x) (Reg 'rsp))
;;             (set (Var 'w) (Var 'y) (Var 'x) (Reg 'rsp))
;;             (set (Var 'w) (Var 'z) (Var 'y) (Reg 'rsp))
;;             (set (Var 'z) (Var 'y) (Reg 'rsp))
;;             (set (Var 't) (Var 'z) (Reg 'rsp))
;;             (set (Var 't) (Var 'z) (Reg 'rsp))
;;             (set (Reg 'rax) (Var 't) (Reg 'rsp))
;;             (set (Reg 'rax) (Reg 'rsp))
;;             (set)))
;;      (list program1 program2))

;; (match (build_interference (uncover_live program1))
;;   [(X86Program _ `(,(cons _ (Block info _))))
;;    (let ([graph (dict-ref info 'conflicts)])
;;      (for*/list ([v (in-vertices graph)])
;;        (list v (for/list ([neighbor (in-neighbors graph v)]) neighbor))))])
