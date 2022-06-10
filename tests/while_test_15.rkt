(let ([x 10])
  (begin
    (begin (set! x 24) (set! x 42))
    x))
