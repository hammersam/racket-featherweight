(define (map [f : (Integer -> Integer)] [vec : (Vector Integer Integer)])
        : (Vector Integer Integer)
  (vector (f (vector-ref vec 0)) (f (vector-ref vec 1))))

(define (double [x : Integer]) : Integer
  (+ x x))

(let ([vec (map double (vector 5 4))])
  (+ 24 (+ (vector-ref vec 0) (vector-ref vec 1))))
