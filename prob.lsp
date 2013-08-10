(defun probability-of (to)
  (if (> to 6)
      0
      (/ (- 6 (- to 1)) 6)))

(defun ! (start &optional (end 1))
  (if (<= start end)
      (if (> end 0)
	  end
	  1)
      (* start (! (- start 1) end))))

(defun ncr (n r)
  (if (= r 0)
      (setf r 1)
      (/ (! n (+ (- n r) 1)) (! r))))

(defun npk (n k)
  (! n (+ (- n k) 1)))
