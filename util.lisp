(in-package #:peas)


(defmacro define-class (class-name superclasses-names &rest slots-names)
  (let ((slots (mapcar #'(lambda (slot-name)
                           (list slot-name
                                 :initarg (make-keyword slot-name)
                                 :initform nil))
                       slots-names)))
    `(defclass ,class-name ,superclasses-names
       (,@slots))))


(defmacro define-constructor (class)
  (let ((*print-case* :upcase)
        (name (intern (format nil "MAKE-~a" class))))
    `(defun ,name (&rest args)
       (apply #'make-instance ',class args))))


(declaim (inline vec2))
(defun vec2 (x y)
  (make-vector2 :x (coerce x 'single-float) :y (coerce y 'single-float)))


(declaim (inline vec3))
(defun vec3 (x y z)
  (make-vector3 :x (coerce x 'single-float) :y (coerce y 'single-float) :z  (coerce z 'single-float)))


(declaim (inline x))
(defun x (v)
  (vector2-x v))


(declaim (inline y))
(defun y (v)
  (vector2-y v))


(declaim (inline z))
(defun z (v)
  (vector3-z v))


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


(defun calculate-location-in-3d (line column board-size)
  (let* ((factor (- 0.5 (/ board-size 2)))
         (x (+ column factor))
         (z (+ line factor)))
    (values (vec3 x 0.5 z)          ; location of the shape in the board, in 3d
            (vec3 x 0.0 z)))) ; location of the shadow in the board, in 3d


(defun draw-grid*-legend (slices camera)
  (let ((cc (char-code #\A)))
    (dotimes (i slices)
      (let ((line-xy (get-world-to-screen (calculate-location-in-3d i -1 slices) camera))
            (column-xy (get-world-to-screen (calculate-location-in-3d -1 i slices) camera)))
        (draw-text (format nil "~a" (1+ i)) (truncate (x line-xy)) (truncate (y line-xy)) 10 +gray+)
        (draw-text (format nil "~a" (code-char (+ cc i))) (truncate (x column-xy)) (truncate (y column-xy)) 10 +gray+)))))


(defun draw-grid* (slices spacing &key (color +lightgray+) (border-color +gray+))
  (let ((half-slices (/ slices 2)))
    ;; top border lines
    (let* ((i (- half-slices))
           (a (* i spacing))
           (b (* half-slices spacing)))
      (draw-line-3d (vec3 a 0 (- b)) (vec3 a 0 b) border-color)
      (draw-line-3d (vec3 (- b) 0 a) (vec3 b 0 a) border-color))
    ;; inner lines
    (loop for i from (1+ (- half-slices)) to (1- half-slices)
          do (let ((a (* i spacing))
                   (b (* half-slices spacing)))
               (draw-line-3d (vec3 a 0 (- b)) (vec3 a 0 b) color)
               (draw-line-3d (vec3 (- b) 0 a) (vec3 b 0 a) color)))
    ;; bottom border lines
    (let* ((i half-slices)
           (a (* i spacing))
           (b (* half-slices spacing)))
      (draw-line-3d (vec3 a 0 (- b)) (vec3 a 0 b) border-color)
      (draw-line-3d (vec3 (- b) 0 a) (vec3 b 0 a) border-color))))
