;; the lifetime of a tuple is irrelevant to the notion of static scoping.
(let ([t1 (vector (vector 44))])
  (let ([x (let ([t (vector 42)])
             (let ([_ (dict-set! t1 0 t)])
               0))])
    (+ x (dict-ref (dict-ref t1 0) 0))))
