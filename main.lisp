(in-package #:peas)


(define-class drawable-with-drawing ())


(defmethod initialize-instance :after ((this drawable-with-drawing) &key game)
  (with-slots (drawables-with-drawing) (or game this)
    (push this drawables-with-drawing)))


(define-class drawable-with-mode-3d ())


(defmethod initialize-instance :after ((this drawable-with-mode-3d) &key game)
  (with-slots (drawables-with-mode-3d) (or game this)
    (push this drawables-with-mode-3d)))


(defgeneric draw (obj context &optional game)
  (:documentation "Draws obj within context (e.g. with-drawing, or with-mode-3d)"))


(define-class actable () frame-counter frame-counter-to-act)


(defmethod initialize-instance :before ((this actable) &key ignored)
  (declare (ignore ignored))
  (with-slots (frame-counter frame-counter-to-act) this
    (setf frame-counter 0)
    (setf frame-counter-to-act 30)))


(defmethod initialize-instance :after ((this actable) &key game)
  (with-slots (actables) game
    (push this actables)))


(defgeneric act (obj &optional game)
  (:documentation "Executes act method if frame-counter is above frame-counter-to-act, and it should return t to reset the frame-counter"))


(defmethod act :around ((this actable) &optional ignored)
  (declare (ignore ignored))
  (with-slots (frame-counter frame-counter-to-act) this
    (when (and (>= frame-counter frame-counter-to-act) ; can act?
               (next-method-p))
      (when (call-next-method)
        (setf frame-counter 0)))
    (incf frame-counter)))


(define-class entity () x y size)


(defmethod print-object ((this entity) stream)
  (print-unreadable-object (this stream :type t)
    (with-slots (x y) this
      (format stream "x: ~a y: ~a" x y))))


(define-class player (entity actable drawable-with-mode-3d))
(define-constructor player)


(defmethod draw ((this player) (context (eql :with-mode-3d)) &optional game)
  (with-slots (x y size) this
    (with-slots (board-size) game
      (multiple-value-bind (shape-location shadow-location)
          (calculate-location-in-3d x y board-size)
        (draw-cube shape-location size size size +red+)
        (draw-cube-wires shape-location size size size +maroon+)
        (draw-plane shadow-location (vec2 size size) +lightgray+)))))


(defmethod act ((this player) &optional game)
  (with-slots (x y) this
    (with-slots (board-size) game
      (when (and (is-key-down +key-a+)
                 (> y 0))
        (decf y)
        (return-from act t))
      (when (and (is-key-down +key-d+)
                 (< y (1- board-size)))
        (incf y)
        (return-from act t))
      (when (and (is-key-down +key-w+)
                 (> x 0))
        (decf x)
        (return-from act t))
      (when (and (is-key-down +key-s+)
                 (< x (1- board-size)))
        (incf x)
        (return-from act t)))))


(define-class mine (entity actable drawable-with-mode-3d))
(define-constructor mine)


(defmethod draw ((this mine) (context (eql :with-mode-3d)) &optional game)
  (with-slots (x y size) this
    (with-slots (board-size) game
      (multiple-value-bind (shape-location shadow-location)
          (calculate-location-in-3d x y board-size)
        (draw-sphere shape-location size +darkgray+)
        (draw-cylinder shadow-location size size 0.0 12 +lightgray+)))))


(defmethod act ((this mine) &optional game)
  (declare (ignore game))
  (with-slots (y) this
    (when (oddp y)
      (decf y)
      (return-from act t))
    (when (evenp y)
      (incf y)
      (return-from act t))))


(define-class game-state (drawable-with-drawing drawable-with-mode-3d)
  actables drawables-with-drawing drawables-with-mode-3d
  board board-size camera)
(define-constructor game-state)


(defmethod draw ((this game-state) (context (eql :with-drawing)) &optional ignored)
  (declare (ignore ignored))
  (with-slots (board-size camera) this
    (draw-grid*-legend board-size camera)))


(defmethod draw ((this game-state) (context (eql :with-mode-3d)) &optional ignored)
  (declare (ignore ignored))
  (with-slots (board-size) this
    (draw-grid* board-size 1.0)
    (let ((shadow-location (nth-value 1 (calculate-location-in-3d (1- board-size) (1- board-size) board-size))))
      (draw-plane shadow-location (vec2 0.9 0.9) '(0 158 47 64))))) ; color +lime+ with transparency


(defun init-game ()
  (let ((game (make-game-state :board-size 3
                               :board (make-array '(3 3))
                               :camera (make-camera3d :position (vec3 5 10 10)
                                                      :target (vec3 0 0 0)
                                                      :up (vec3 0 1 0)
                                                      :fovy 35.0
                                                      :type +camera-perspective+))))
    (make-player :x 0 :y 0 :size 0.7 :frame-counter-to-act 15 :game game)
    (make-mine :x 1 :y 1 :size 0.35 :frame-counter-to-act 30 :game game)
    (make-mine :x 2 :y 2 :size 0.15 :frame-counter-to-act 15 :game game)
    game))


(defun run-game (game)
  (with-slots (actables drawables-with-drawing drawables-with-mode-3d camera) game
    (dolist (i actables)
      (act i game))
    (with-drawing
      (clear-background +raywhite+)
      (dolist (i drawables-with-drawing)
        (draw i :with-drawing game))
      (with-mode-3d (camera)
        (dolist (i drawables-with-mode-3d)
          (draw i :with-mode-3d game))))))


(defun run ()
  (with-window (800 600 "Bag of Peas")
    (set-target-fps 60)
    (let ((game (init-game)))
      (loop
        (when (window-should-close)     ; dectect window close button or ESC key
          (return))
        (run-game game)))))
