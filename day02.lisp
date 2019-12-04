(in-package :aoc-2019)

(defun load-day02-input ()
  (with-open-file (stream "projects/aoc-2019/data/day02.input.txt")
    (loop for line = (read-line stream nil)
          while line
          append (mapcar 'parse-integer (uiop:split-string line :separator ",")))))

(defparameter *day02-input* (load-day02-input))

(defun execute-intcode (program)
  (loop with cursor = 0
        for opcode = (aref program cursor)

        when (= opcode 1) do        
        
        (setf (aref program (aref program (+ 3 cursor)))
              (+ (aref program (aref program (+ 1 cursor)))
                 (aref program (aref program (+ 2 cursor)))))
        (incf cursor 4)

        when (= opcode 2) do
        (setf (aref program (aref program (+ 3 cursor)))
              (* (aref program (aref program (+ 1 cursor)))
                 (aref program (aref program (+ 2 cursor)))))
        (incf cursor 4)

        when (= opcode 99) return program))

(assert (equalp #(2 0 0 0 99)
                (execute-intcode #(1 0 0 0 99))))

(assert (equalp #(30 1 1 4 2 5 6 0 99)
                (execute-intcode #(1 1 1 4 99 5 6 0 99))))

(defparameter *day02-result01*
  (let ((program (make-array (length *day02-input*) :initial-contents *day02-input*)))
    (setf (aref program 1) 12)
    (setf (aref program 2) 2)

    
    (aref (execute-intcode program) 0)))

(assert (= 3306701 *day02-result01*))

;;; challenge 2

(defun patch-array (array &rest patch-pairs)
  (loop for (a b) on patch-pairs by #'cddr
        do (setf (aref array a) b))
  array)

(assert (equalp (patch-array #(1 2 3 4) 1 4 2 9) #(1 4 9 4)))

(defun intcode-patch-execute-return (program &rest patches)
  ;;; patch, execute and return address 0 all in one call.
  (aref (execute-intcode (apply #'patch-array program patches)) 0))

(assert (= *day02-result01* (intcode-patch-execute-return (coerce *day02-input* 'vector) 1 12 2 2)))

(defparameter *day02-result02*
  (loop named search-result
        for noun from 0 to 99
        do (loop for verb from 0 to 99
                 when (= 19690720 (intcode-patch-execute-return (coerce *day02-input* 'vector) 1 noun 2 verb))
                 do (return-from search-result (+ (* noun 100) verb)))))

(assert (= 7621 *day02-result02*))
