;; the return value has to be 42 otherwise it fails
(let ([x (and #t #f)])
  42)
