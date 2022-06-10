(let ([x (let ([y 10]) (begin (set! y 42) y))])
  x)
