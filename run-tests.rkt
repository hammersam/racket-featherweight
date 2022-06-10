#lang racket

(require "utilities.rkt")
(require "interp-Lvec-prime.rkt")
(require "type-check-Lvec.rkt")
(require "interp-Lfun-prime.rkt")
(require "type-check-Lfun.rkt")
(require "compiler.rkt")
;; (debug-level 1)
;; (AST-output-syntax 'concrete-syntax)

;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))

;; (interp-tests "var" type-check-Lvec compiler-passes interp-Lvec-prime "var_test" (tests-for "var"))
;; (interp-tests "cond" type-check-Lvec compiler-passes interp-Lvec-prime "cond_test" (tests-for "cond"))
;; (interp-tests "while" type-check-Lvec compiler-passes interp-Lvec-prime "while_test" (tests-for "while"))
;; (interp-tests "vectors" type-check-Lvec compiler-passes interp-Lvec-prime "vectors_test" (tests-for "vectors"))
;; (compiler-tests "var" type-check-Lvec compiler-passes "var_test" (tests-for "var"))
;; (compiler-tests "cond" type-check-Lvec compiler-passes "cond_test" (tests-for "cond"))
;; (compiler-tests "while" type-check-Lvec compiler-passes "while_test" (tests-for "while"))
;; (compiler-tests "vectors" type-check-Lvec compiler-passes "vectors_test" (tests-for "vectors"))

(interp-tests "functions" type-check-Lfun compiler-passes interp-Lfun-prime "functions_test" (tests-for "functions"))
