;#! /usr/bin/clisp

(defun is-a-factor (number potential-factor)
  (if (= (mod number potential-factor) 0) T NIL))

;True if number is not divisable by the passed in list of primes
;Primes *MUST* be the set of all primes below number
(defun is-a-prime (primes number)
  (if (eq primes nil)
    T ;None of our primes were factors, so number must be a prime
    (if (is-a-factor number (first primes))
      nil
      (is-a-prime (rest primes) number))))
  
;Assumes the list is a sorted list of primes
(defun compute-next-prime (&optional (primes '(2)) (number-to-try (+ 1 (first (last primes)))))
  (if (is-a-prime primes number-to-try)
    (append primes (list number-to-try))
    (compute-next-prime primes (+ 1 number-to-try))))

(defun compute-all-primes-below (end &optional (primes (compute-next-prime)))
  (if (< (first (last primes)) end)
    (compute-all-primes-below end (compute-next-prime primes))
    (subseq primes 0 (- (list-length primes) 1))))

(defun reduce-num (number aprime)
  (if (is-a-factor number aprime)
    (reduce-num (/ number aprime) aprime)
    number))

(defun list-minus (list-a &rest lists)
  (dolist (list-b lists)
    (dolist (x list-b)
      (setf list-a (remove x list-a))))
  list-a)
  
;Returns a list of all primes calculated and a subset of that list which contains the prime factors for number
(defun get-prime-factors (number &optional (primes (compute-next-prime nil)) factors nonfactors)
  (let ((reduced 0))
  (dolist (prime (list-minus primes factors nonfactors))
    (setf reduced (reduce-num number prime))
    (if (= reduced number)
      (setf nonfactors(append nonfactors (list prime)))
      (setf factors (append factors (list prime))))
    (setf number reduced)))
  ;(format t "number is now ~d~%" number)
  (if (= 1 number)
    (list primes factors)
    (get-prime-factors number (compute-next-prime primes) factors nonfactors)))
