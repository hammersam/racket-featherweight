;; Operands must be atomic for explicate_control pass.
(let ([x (let ([x.1 10]) (+ x.1 2))])
  (- x 10))
