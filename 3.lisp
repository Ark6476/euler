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
(defun compute-next-prime (primes)
  (if (eq primes nil)
    (list 2) ;1 isn't a prime number so don't even think about it
    (compute-next-prime-helper primes (+ (first (last primes)) 1))))

(defun compute-next-prime-helper (primes number-to-try)
  (if (is-a-prime primes number-to-try)
    (append primes (list number-to-try))
    (compute-next-prime-helper primes (+ 1 number-to-try))))

(defun compute-all-primes-below (end primes)
  (if (or (eq nil primes) (< (first (last primes)) end))
    (compute-all-primes-below end (compute-next-prime primes))
    primes))

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
  (dolist (prime (list-minus primes factors nonfactors))
    (setf reduced (reduce-num number prime))
    (if (= reduced number)
      (setf nonfactors(append nonfactors (list prime)))
      (setf factors (append factors (list prime))))
    (setf number reduced))
  ;(format t "number is now ~d~%" number)
  (if (= 1 number)
    (list primes factors)
    (get-prime-factors number (compute-next-prime primes) factors nonfactors)))
