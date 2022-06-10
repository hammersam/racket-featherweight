(let ([x (let ([x #f]) x)])
  (let ([x (let ([x #t]) x)])
    42))
