(defun our-member (obj lst)
  (cond ((atom lst) nil)
        ((eql obj (car lst)) lst)
        (t (our-member obj (cdr lst)))))

(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb 28)
    (otherwise "unknown month")))

(defun super ()
  (catch 'abort
    (sub)
    (format t "we'll never see this.")))

(defun sub ()
  (throw 'abort 99))

(defun ssuper ()
  (super)
  (format t "we'll still gonna see this"))
