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

(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args)
  (apply (combiner (car args)) args))

(defun our-complement (fn)
  (lambda (&rest args)
    (not (apply fn args))))

(defun compose (&rest fns)
  (destructuring-bind (x . y) (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (v f)
                (funcall f v))
              y
              :initial-value (apply x args)))))

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        (lambda (&rest args)
          (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
        (lambda (&rest args)
          (and (apply fn args) (apply conj args))))))

(defun curry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))))


(defun rcurry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args2 args))))

(defun always (x)
  (lambda (&rest args) x))
