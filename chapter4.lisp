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
