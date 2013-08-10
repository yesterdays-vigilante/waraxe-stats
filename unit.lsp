(load (merge-pathnames *library-location* "prob.lsp"))
(load (merge-pathnames *library-location* "equip.lsp"))

(defclass unit ()
  ((weapon-skill
    :initform 3
    :initarg :ws
    :accessor ws-of)
   (ballistic-skill 
    :initform 3
    :initarg :bs
    :accessor bs-of)
   (strength
    :initform 3
    :initarg :strength
    :accessor strength-of)
   (toughness 
    :initform 3
    :initarg :toughness
    :accessor toughness-of)
   (initiative 
    :initform 3
    :initarg :init
    :accessor initiative-of)
   (attacks 
    :initform 3
    :initarg :attacks
    :accessor attacks-of)
   (number 
    :initform 1
    :initarg :number
    :accessor number-of)
   (ranks 
    :initform 1
    :initarg :ranks
    :accessor ranks-of)
   (save-modifier 
    :initform 0
    :initarg :modifier
    :accessor save-mod-of)
   (save 
    :initform 7
    :initarg :save
    :accessor save-of)
   (ranged-weapon
    :initform (make-instance 'ranged-weapon)
    :initarg :ranged
    :accessor ranged-weapon-of)))

(defmethod calc-to-hit-ranged ((firer unit) &optional (mod 0))
  (let ((th (- (- 7 (bs-of firer)) mod)))
    (if (< th 2)
	2
        th)))

(defun calc-to-wound (strength toughness)
   (let ((delta (+ 4 (- toughness strength))))
    (if (> delta 1)
	(if (> delta 6)
	    (if (> delta 7)
		7
		6)
	    delta)
	2)))

(defmethod calc-to-wound-target ((firer unit) (target unit))
  (calc-to-wound (strength-of firer) (toughness-of target)))

(defmethod total-save-mod-of ((firer unit))
  (if (< (strength-of firer) 4)
      (save-mod-of firer)
      (- (+ (- (strength-of firer) 3) (save-mod-of firer)))))

(defun modified-save (save mod)
  (- save mod))

(defmethod save-of-target ((firer unit) (target unit))
  (modified-save (save-of target) (total-save-mod-of firer)))

(defun prob-to-wound (to-wound to-save)
  (* (probability-of to-wound) (- 1 (probability-of to-save))))

(defmethod prob-to-wound-target ((firer unit) (target unit))
  (prob-to-wound (calc-to-wound-target firer target) 
		 (save-of-target firer target)))

(defvar firer (make-instance 'unit))
(defvar target (make-instance 'unit))