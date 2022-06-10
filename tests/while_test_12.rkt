(let ([x 12])
  (let ([y 32])
    (let ([z (if (not #t) (set! x 22) (set! y 10))])
      (+ x y))))
