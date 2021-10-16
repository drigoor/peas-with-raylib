(in-package #:peas)


(defmacro define-constructor (class)
  (let ((*print-case* :upcase)
        (name (intern (format nil "MAKE-~a" class))))
    `(defun ,name (&rest args)
       (apply #'make-instance ',class args))))


(declaim (inline vec2))
(defun vec2 (x y)
  (make-vector2 :x (coerce x 'single-float) :y (coerce y 'single-float)))


(declaim (inline x))
(defun x (v)
  (vector2-x v))


(declaim (inline y))
(defun y (v)
  (vector2-y v))


(defun vector2-rotate (v angle)
  (let ((x (x v))
        (y (y v))
        (cos (cos angle))
        (sin (sin angle)))
    (vec2 (- (* x cos) (* y sin))
          (+ (* y cos) (* x sin)))))


(defun vector2-add (v1 v2)
  (vec2 (+ (x v1) (x v2))
        (+ (y v1) (y v2))))


(defun vector2-scale (v scale)
  (vec2 (* (x v) scale)
        (* (y v) scale)))


(defun <-vec2 (&rest args)
  (loop for (x y) on args by #'cddr
        collect (vec2 x y)))


(defun random-between (start end)
  (+ start (random (+ 1 (- end start)))))


(defun vec2-from-speed-and-direction (speed direction)
  (vec2 (* speed (cos direction))
        (* speed (sin direction))))
