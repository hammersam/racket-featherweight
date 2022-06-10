(define (binary2decimal [x7 : Integer] [x6 : Integer] [x5 : Integer]
                        [x4 : Integer] [x3 : Integer] [x2 : Integer]
                        [x1 : Integer] [x0 : Integer]) : Integer
  (let ([vec (vector x0 x1 x2 x3 x4 x5 x6 x7)])
    (let ([pos 0])
      (let ([power 1])
        (let ([result 0])
          (begin (while (< pos 8)
                   (begin (if (eq? (vector-ref vec pos) 1)
                              (set! result (+ result power))
                              (void))
                          (set! pos (+ pos 1))
                          (set! power (+ power power))))
                 result))))))

(binary2decimal 0 0 1 0 1 0 1 0)
