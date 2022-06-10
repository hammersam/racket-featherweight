(let ([x 10])
  (begin
    (let ([y (begin (set! x 42) x)]) y)
    x))
