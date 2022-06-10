(let ([y (if #t
             (read)
             (if (eq? (read) 0)
                 777
                 (let ([x (read)])
                   (+ x 1))))])
  (+ y 2))
