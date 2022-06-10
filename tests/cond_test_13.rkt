(let ([x (let ([y (if (eq? #t #f) (and 1 #t) (not #t))]) y)])
  42)
