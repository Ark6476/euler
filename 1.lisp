#! /usr/bin/clisp
(format t "~&~S~&" *args*)

(defun sumToOne (x)
  (if (= x 0) 0
    (+ x (sumToOne (- x 1)))))

# 3 3 => 3 + 6 + 9
(defun multipleSummation (x y)
  (* x (sumToOne y)))

# 10 3 => 3
(defun getLargestMultiplier (x y)
  (nth-value 0 (floor x y)))

(defun multipleSum (cap a)
  (multipleSummation a (getLargestMultiplier cap a)))

#I am a god
(defun multiplesSum (cap a b)
  (-
   (+ (multipleSum cap a) (multipleSum cap b))
   (multipleSum cap (* a b))))



