#! /usr/bin/clisp

# even + even = even
# even + odd = odd
# odd + odd = even


(defun sum-of-even-fibs (cap)
       (compute-sum-of-even-fibs cap 1 1 NIL NIL))

(defun compute-sum-of-even-fibs (cap a b aIsEven bIsEven)
  (if (>= b cap)
      0 ;We've hit the cap
      (if (or (and aIsEven bIsEven) (and (not aIsEven) (not bIsEven)))
	  (+ (+ a b) (compute-sum-of-even-fibs cap b (+ a b) bIsEven T))
	  (compute-sum-of-even-fibs cap b (+ a b) bIsEven NIL))))
      
