(in-package :aoc-2019)

(defun load-day01-input ()
  (with-open-file (stream "projects/aoc-2019/data/day01.input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

(defparameter *day01-input* (load-day01-input))


;;; Challenge 1

(defun calculate-fuel-from-mass (mass)
  (max 0 (- (floor mass 3) 2)))

(assert (= 966 (calculate-compound-fuel-from-mass 1969)))

(defun fuel-for-modules (modules)
  (loop for m in modules
        sum (calculate-fuel-from-mass m)))

(defparameter *day01-result01* (fuel-for-modules *day01-input*))
(assert (= 3502510 *day01-result01*))


;;; Challenge 2

(defun calculate-compound-fuel-from-mass (mass)
  (loop for val = (calculate-fuel-from-mass mass) then (calculate-fuel-from-mass val)
        while (> val 0)
        sum val))

(assert (= 50346 (calculate-compound-fuel-from-mass 100756)))

(defun compound-fuel-for-modules (modules)
  (loop for m in modules
        sum (calculate-compound-fuel-from-mass m)))

(defparameter *day01-result02* (compound-fuel-for-modules *day01-input*))
(assert (= 5250885 *day01-result02*))
