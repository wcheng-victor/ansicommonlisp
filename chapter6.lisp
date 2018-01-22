(defun primo (lst)
  "a test function , its function is like car"
  (car lst))

(defun (setf primo) (val lst)
  (setf (car lst) val))

(defun our-funcall (fn &rest arguments)
  (apply fn arguments))

(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (reverse acc)))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val
            (push val acc))))
    (reverse acc)))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (x (cdr lst))
          (let ((score (funcall fn x)))
            (if (> score max)
                (progn (setf wins x)
                       (setf max score)))))
        (values wins max))))
