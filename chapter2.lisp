(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql obj (car lst))
          lst
          (our-member obj (cdr lst)))))

(defun askanum ()
  (format t "please enter an number~%")
  (let ((val (read)))
    (if (numberp val)
        val
        (askanum))))

(defun show-square (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A : ~A~%" i (* i i))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length-rec (lst)
  (if (null lst)
      0
      (+ 1 (our-length-rec (cdr lst)))))
