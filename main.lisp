(in-package #:peas)


(define-constant +pi+ (coerce pi 'single-float))


(define-constant +screen-width+ 640)
(define-constant +screen-height+ 480)


(defclass shape ()
  ((location :initarg :location
             :initform nil
             :accessor location)
   (velocity :initarg :velocity
             :initform nil              ; speed -> x, direction -> y
             :accessor velocity)
   (orientation :initarg :orientation
                :initform 0.0
                :accessor orientation)
   (orientation-speed :initarg :orientation-speed
                      :initform 0.0
                      :accessor orientation-speed)
   (thickness :initarg :thickness
              :initform 1.0
              :accessor thickness)
   (color :initarg :color
          :initform +lightgray+
          :accessor color)
   (points :initarg :points
           :initform nil
           :accessor points)))


(defun make-shape (&rest args)
  (apply #'make-instance 'shape args))


(defmethod draw-shape ((shape shape))
  (with-slots (location orientation thickness color points) shape
    (let ((start nil))
      (dolist (pt points)
        (let ((end (vector2-rotate pt orientation)))
          (when start
            (draw-line-ex (vector2-add location start)
                          (vector2-add location end)
                          thickness
                          color))
          (setf start end))))
    (draw-circle-v location thickness +red+)))


(defun spawn-player (location)
  (make-shape :location location
              :orientation-speed 0.07
              :thickness 2.0
              :points (<-vec2  20   0
                              -20  15
                              -10   0
                              -20 -15
                               20   0)))


(define-constant +asteroid-num-points+ 10)
(define-constant +asteroid-rad+ 15)
(define-constant +asteroid-rad-plus+ 4)
(define-constant +asteroid-rad-minus+ 6)
(define-constant +asteroid-max-vel+ 0.5)
(define-constant +asteroid-min-vel+ 0.1)
(define-constant +asteroid-max-rot+ 0.03)


(defun generate-asteroid-points (scale)
  (let ((result nil))
    ;; first point
    (push (vec2 (/ +asteroid-rad+ scale) 0)
          result)
    ;; midle points
    (dotimes (index (- +asteroid-num-points+ 2)) ; index will start in 1 (see 1+ bellow)
      (let ((speed (/ (random-between (- +asteroid-rad+ +asteroid-rad-minus+)
                                      (+ +asteroid-rad+ +asteroid-rad-plus+))
                      scale))
            (direction (* (1+ index)
                          (/ (* +pi+ 2)
                             +asteroid-num-points+))))
        ;; (push (vector2-scale (vec2-from-speed-and-direction speed direction) size)
        (push (vec2-from-speed-and-direction speed direction)
              result)))
    ;; last point
    (push (vec2 (/ +asteroid-rad+ scale) 0)
          result)
    result))


(defun spawn-asteroid (location scale)
  (make-shape :location location
              :velocity (vec2 (random-between +asteroid-min-vel+ +asteroid-max-vel+)
                              (random-between 0 (* 2 +pi+)))
              :orientation (random-between 0 +pi+)
              :orientation-speed (- (* (random 2.0) (* 2 +asteroid-max-rot+)) +asteroid-max-rot+)
              :thickness 2.0
              :points (generate-asteroid-points scale)))


(defun spawn-asteroids ()
  (list (spawn-asteroid (vec2 100 200) 0.5)
        (spawn-asteroid (vec2 100 300) 0.6)
        (spawn-asteroid (vec2 100 400) 0.8)))


(defun run ()
  (let ((player (spawn-player (vec2 (/ +screen-width+ 2)
                                    (/ +screen-height+ 2))))
        (asteroids (spawn-asteroids)))
    (with-window (+screen-width+ +screen-height+ "peas")
      (set-target-fps 60)               ; Set our game to run at 60 FPS
      (loop
        (when (window-should-close) (return)) ; dectect window close button or ESC key

        (with-slots (orientation orientation-speed) player
          (when (or (is-key-down +key-a+)
                    (is-key-down +key-left+))
            (decf orientation orientation-speed))
          (when (or (is-key-down +key-d+)
                    (is-key-down +key-right+))
            (incf orientation orientation-speed)))
        (with-drawing
          (clear-background +raywhite+)
          (draw-fps 20 20)
          (draw-shape player)
          (dolist (asteroid asteroids)
            (draw-shape asteroid))
          (draw-text "Just an initial bag of peas" 0 0 20 +lightgray+))))))
