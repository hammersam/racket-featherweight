;; dict-set!
(let ([t1 (vector 3 7)])
  (let ([t2 t1])
    (let ([_ (dict-set! t2 0 42)])
      (dict-ref t1 0))))
