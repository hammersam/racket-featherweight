(let ([x 5])
  (let ([y 10])
    (let ([res 0])
      (begin
        (while (> y 0)
          (begin
            (set! res (+ res x))
            (set! y (- y 1))))
        res))))
