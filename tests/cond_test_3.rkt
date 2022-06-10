(if (let ([x (let ([x (let ([x 42]) x)]) x)]) #t)
    (let ([x 42]) x)
    (let ([x 42]) x))
