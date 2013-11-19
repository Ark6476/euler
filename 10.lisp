(load (compile-file "3.lisp"))


;(defun sum-of-primes-below (number)
;  (apply '+ (compute-all-primes-below number)))

; Sum of all primes below 2*10^6 is 142913828922
; It took a long time to compute though, so a better solution would be to *not*
; store all the primes as you go. Instead have a different is-a-prime function
; that doesn't need a list if prior primes. This would be a lot less memory intensive.
(defun sum-of-primes-below (number &optional (primes '(2)) (sum 0))
  (if (< (first (last primes)) number)
    (sum-of-primes-below number (compute-next-prime primes) (+ sum (first (last primes))))
    sum))
