(defun mirror? (s)
  (and (not (null s))
       (let ((len (length s)))
         (and (evenp len)
              (do ((forward 0 (+ forward 1))
                   (backward (- len 1) (- backward 1)))
                  ((or (> forward backward)
                       (not (eql (elt s forward)
                                 (elt s backward))))
                   (> forward backward)))))))

(defun second-word (s)
  (let ((p1 (+ (position #\  s) 1)))
    (subseq s p1 (position #\  s :start p1))))

(defstruct (point
            (:conc-name p)
            (:print-function print-point))
  (x 1)
  (y 0))

(defun print-point (p stream depth)
  (format stream "#<~A ~A>" (px p) (py p)))

(defun binary-search (obj vec)
  (let ((len (length vec)))
    (if (= 0 len)
        nil
        (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (= range 0)
        (if (eql obj (aref vec start))
            obj
            nil)
        (let ((mid (round (/ (+ start end) 2))))
          (if (> obj (aref vec mid))
              (finder obj vec (+ mid 1) end)
              (if (< obj (aref vec mid))
                  (finder obj vec start (- mid 1))
                  obj))))))

(defun constituent (p)
  (and (graphic-char-p p)
       (not (char= p #\ ))))

(defun token (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if (lambda (x) (not (funcall test x))) str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (token str test p2)
                    nil)))
        nil)))

(defun parse-date (str)
  (let ((tokens (token str #'constituent 0)))
    (list (parse-integer (first tokens))
          (parse-month (second tokens))
          (parse-integer (third tokens)))))

(defun parse-month (str)
  (let ((p (position str month-names :test #'string-equal)))
    (if p
        (+ p 1)
        nil)))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defstruct (node (:print-function (lambda (n s d)
                                    (format s "#<~A>" (node-elt n)))))
  elt
  (l nil)
  (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                 :elt (node-elt bst)
                 :l (bst-insert obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt (node-elt bst)
                 :l (node-l bst)
                 :r (bst-insert obj (node-r bst) <)))))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

(defun percolate (bst)
  (cond ((null (node-l bst))
         (if (null (node-r bst))
             nil
             (rperc bst)))
        ((null (node-r bst))
         (lperc bst))
        (t (if (zerop (random 2))
               (rperc bst)
               (lperc bst)))))

(defun rperc (bst)
  (make-node :elt (node-elt (node-r bst))
             :l (node-l bst)
             :r (percolate (node-r bst))))

(defun lperc (bst)
  (make-node :elt (node-elt (node-l bst))
             :l (percolate (node-l bst))
             :r (node-r bst)))

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst)
            (if (funcall < obj elt)
                (make-node :elt elt
                           :l (bst-remove obj (node-l bst) <)
                           :r (node-r bst))
                (make-node :elt elt
                           :l (node-l bst)
                           :r (bst-remove obj (node-r bst) <)))))))
