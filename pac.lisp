(ql:quickload :series)
(in-package :pac)

(defun impfun0 (set1)
  (declare (optimize (safety 0) (speed 3)))
  (loop :for i :in set1
              :if (plusp i)
                :summing i) ;; fast
  )

(defun impfun1 (set1)
  (declare (optimize (safety 0) (speed 3)))
  (collect-sum
         (choose-if #'plusp
                    (scan set1))) ;; fast
  )

(defun impfun2 (set1)
  (declare (optimize (safety 0) (speed 3)))
  (reduce #'+ (remove-if-not #'plusp set1)) ;;slow
  )

