(let ([x #t])
  (let ([y (begin (set! x #f) (not x))])
    (begin
      (set! y (not y))
      (if y 10 42))))
