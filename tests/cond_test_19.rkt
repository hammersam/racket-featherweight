(let ([x (and (and (and (and #t #f) #f) #f) #f)])
  (let ([x (or (or (or (or #f #t) #t) #t) #t)])
    42))
