(let ([x 10])
  (begin
    (if #t (set! x 42) (set! x 24))
    x))
