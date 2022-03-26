;; patch_instructions test1
(let ((a 10))
  (let ((b a))
    (let ((c b))
      (let ((d c))
        (let ((e (read)))
          (+ 32 d))))))
