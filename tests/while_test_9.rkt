(let ([x #t])
  (if (begin (set! x #f) x)
      10
      42))
