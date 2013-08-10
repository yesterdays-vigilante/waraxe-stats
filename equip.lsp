(defclass ranged-weapon ()
  ((strength
    :initform 3
    :initarg :strength
    :accessor strength-of)
   (shots
    :initform 1
    :initarg :shots
    :accessor shots-of)
   (to-hit-modifier
    :initform 0
    :initarg :to-hit
    :accessor modifier-of)))