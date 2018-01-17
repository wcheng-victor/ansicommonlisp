(defun our-listp (lst)
  (or (null lst) (consp lst)))

(defun our-atom (obj)
  (not (consp obj)))

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

(defun compress (x)
  (if (atom x)
      x
      (compr (car x) 1 (cdr x))))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts n elt))
      (if (eql elt (car lst))
          (compr elt (+ n 1) (cdr lst))
          (cons (n-elts n elt)
                (compress lst)))))

(defun n-elts (n elt)
  (if (> n 1)
      (list n elt)
      elt))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(defun uncompress (lst)
  (if (null lst)
      nil
      (append
       (if (consp (car lst))
           (list-of (car (car lst))
                    (cadr (car lst)))
           (list (car lst)))
       (uncompress (cdr lst)))))
