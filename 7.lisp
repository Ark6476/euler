(load (compile-file "3.lisp"))
 
(defun compute-nth-prime (n &optional primes)
  (if (<= n (list-length primes))
    primes
    (compute-nth-prime n (compute-next-prime primes))))
