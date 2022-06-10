;; 51 / 3
(let ([x 51])
  (let ([y 3])
    (let ([quot 0])
      (begin
        (while (>= x y)
          (begin
            (set! quot (+ quot 1))
            (set! x (- x y))))
        quot))))
