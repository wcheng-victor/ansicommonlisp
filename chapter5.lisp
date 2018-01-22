
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

(defconstant month
  (let ((mon (reverse '(31 28 31 30 31 30 31 31 30 31 30 31))))
    (cons 0 (reverse
             (maplist (lambda (x)
                        (apply #'+ x))
                      mon)))))

(defconstant yzero 2000)

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (+ (- d 1)
     (month-num m y)
     (year-num y)))

(defun month-num (m y)
  (+ (elt month (- m 1))
     (if (and (leap? y) (> m 2))
         1
         0)))

(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
        (dotimes (i (- y yzero) d)
          (incf d (year-days (+ yzero i))))
        (dotimes (i (- yzero y) (- d))
          (incf d (year-days (+ y i)))))))

(defun year-days (y)
  (if (leap? y)
      366
      365))

(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (d m) (num-month left y)
      (values d m y))))

(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))

(defun num-month (n y)
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t (nmon ( - n 1))))
      (nmon n)))

(defun nmon (n)
  (let ((m (position n month :test #'<)))
    (values m (+ 1 (- n (elt month (- m 1)))))))

(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))
