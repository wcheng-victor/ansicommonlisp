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

(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) (cdr lst))))

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

(defun our-subst (new old tree)
  (if (eql old tree)
      new
      (if (atom tree)
          tree
          (cons (our-subst new old (car tree))
                (our-subst new old (cdr tree))))))

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
           lst
           (our-member-if fn (cdr lst)))))

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid len)))))))

(defun nthmost (n lst)
  (nth (- n 1) (sort lst #'>)))

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (obj lst)
      (push obj acc))
    acc))

(defun proper-list? (lst)
  (or (null lst)
      (and (consp lst)
           (proper-list? (cdr lst)))))

(defun our-assoc (key alist)
  (and (consp alist)
       (not (null alist))
       (let ((pair (car alist)))
         (if (and (consp pair)
                  (eql (car pair) key))
             pair
             (our-assoc key (cdr alist))))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null net)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end (append (cdr queue)
                               (new-paths path node net)) net))))))

(defun new-paths (path node net)
  (mapcar (lambda (n) (cons n path))
          (cdr (assoc node net))))
