(let ([x #t])
  (let ([x #f])
    (let ([x (let ([x #t]) x)])
      42)))
