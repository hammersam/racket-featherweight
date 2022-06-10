(let ([n 10])
  (let ([bif 0])
    (let ([fib 1])
      (let ([i 1])
        (begin
          (while (< i n)
            (let ([new-fib (+ bif fib)])
              (begin
                (set! bif fib)
                (set! fib new-fib)
                (set! i (+ i 1)))))
          fib)))))
; 1 1 2 3 5 8 13 21 34 55
