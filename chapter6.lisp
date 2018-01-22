(defun primo (lst)
  "a test function , its function is like car"
  (car lst))

(defun (setf primo) (val lst)
  (setf (car lst) val))

(defun our-funcall (fn &rest arguments)
  (apply fn arguments))

