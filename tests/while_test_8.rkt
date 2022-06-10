;; 3^5
(let ([base 2])
  (let ([power 7])
    (let ([res 1])
      (let ([odm 0])
        (begin
          (while (> power 0)
            (begin
              (if (eq? (let ([x power])
                         (let ([y 2])
                           (let ([quot 0])
                             (begin
                               (while (>= x y)
                                 (begin
                                   (set! quot (+ quot 1))
                                   (set! x (- x y))))
                               (set! power quot)
                               (set! odm x)
                               odm))))
                       1)
                  (set! res (let ([x res])
                              (let ([y base])
                                (let ([res 0])
                                  (begin
                                    (while (> y 0)
                                      (begin
                                        (set! res (+ res x))
                                        (set! y (- y 1))))
                                    res)))))
                  (void))
              (set! base (let ([x base])
                           (let ([y base])
                             (let ([res 0])
                               (begin
                                 (while (> y 0)
                                   (begin
                                     (set! res (+ res x))
                                     (set! y (- y 1))))
                                 res)))))))
          res)))))
