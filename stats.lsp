2(defvar *library-location* "/home/zedd/Documents/lisp/stats/")
(load (merge-pathnames *library-location* "prob.lsp"))
(load (merge-pathnames *library-location* "unit.lsp"))

(defun average-hits (to-hit num-shots)
  (* (probability-of to-hit) num-shots))

(defmethod average-hits-of ((firer unit) &optional (mod 0))
  (average-hits (calc-to-hit-ranged firer mod) (number-of firer)))

(defun average-wounds (hits prob-to-wound)
  (* hits prob-to-wound))

(defmethod average-wounds-of ((firer unit) (target unit))
  (average-wounds (average-hits-of firer) 
		  (prob-to-wound-target firer target)))

(defun ranged-dist-of (total n prob-to-hit prob-to-wound)
  (let ((prob (* prob-to-hit prob-to-wound)))
    (* (expt prob n) (expt (- 1 prob) (- total n)) (ncr total n))))

(defun bt-to-wound (strength toughness prev-prob n)
  (if (= n 0)
      prev-prob
      (* (bt-to-wound strength toughness prev-prob (- n 1)) 
	 (probability-of (calc-to-wound (- strength n) toughness)))))

(defun bt-dist-of (strength toughness prob-to-hit n last)
  (if (= n 0)
      (+ (- 1 prob-to-hit) 
	 (* prob-to-hit (- 1 (probability-of (calc-to-wound strength toughness)))))
      (* (bt-to-wound strength toughness prob-to-hit (- n 1)) 
	 (probability-of (calc-to-wound strength toughness)) 
	 (if (not last) 
	     (- 1 (probability-of (calc-to-wound (- strength n) toughness)))
	     1))))

(defmethod nth-bt-dist-for (n (firer unit) (target unit) last)
  (bt-dist-of (strength-of firer) 
	      (toughness-of target) 
	      (probability-of (calc-to-hit-ranged firer)) n last))

(defmethod bt-dist-for ((firer unit) (target unit))
  (let ((n (if (> (ranks-of target) 6) 6 (ranks-of target))))
    (loop for i from 0 to n collecting 
	 (list i (nth-bt-dist-for i firer target (= i n))))))

(defmethod nth-ranged-dist-for (n (firer unit) (target unit) &optional (to-hit-mod 0))
  (ranged-dist-of (number-of firer) n 
	   (probability-of (calc-to-hit-ranged firer to-hit-mod)) 
	   (prob-to-wound-target firer target)))

(defmethod ranged-dist-for ((firer unit) (target unit) &optional (to-hit-mod 0))
  (loop for i from 0 to (number-of firer) collecting 
       (list i (nth-ranged-dist-for i firer target to-hit-mod))))

(defun percentage-from-list (list)
  (loop for n in list collecting 
       (list (nth 0 n) (* (nth 1 n) 100))))

(defun print-list (list)
  (format t "~:{~a~3t~4$%~%~}" list))

(defmethod print-ranged-dist-for ((firer unit) (target unit))
  (print-list (percentage-from-list (ranged-dist-for firer target))))
