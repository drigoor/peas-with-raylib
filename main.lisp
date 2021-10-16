(in-package #:peas)


(defparameter +board-size+ 3)


(defun calculate-location-in-3d (line column board-size)
  (let* ((factor (- 0.5 (/ board-size 2)))
         (x (+ column factor))
         (z (+ line factor)))
    (values (vec3 x 0.5 z)          ; location of the shape in the board, in 3d
            (vec3 x 0.0 z)))) ; location of the shadow in the board, in 3d


(defun draw-player (line column board-size &key (size 0.7))
  (multiple-value-bind (shape-location shadow-location)
      (calculate-location-in-3d line column board-size)
    (draw-cube shape-location size size size +red+)
    (draw-cube-wires shape-location size size size +maroon+)
    (draw-plane shadow-location (vec2 size size) +lightgray+)))


(defun draw-mine (line column board-size &key (size 0.35))
  (multiple-value-bind (shape-location shadow-location)
      (calculate-location-in-3d line column board-size)
    (draw-sphere shape-location size +darkgray+)
    (draw-cylinder shadow-location size size 0.0 12 +lightgray+)))


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


(defun draw-all (camera)
  (with-drawing
    (clear-background +raywhite+)
    (draw-grid*-legend +board-size+ camera)
    (with-mode-3d (camera)
      (draw-grid* +board-size+ 1.0)
      (draw-player 1 1 +board-size+)
      (draw-mine 0 0 +board-size+))
    (draw-text "Welcome to the bag of peas" 10 40 20 +darkgray+)
    (draw-fps 10 10)))


(defparameter *lixo* 0.0) ; accumulate frame-time to allow to do something at a given interval of time


(defun handle-input ()
  (incf *lixo* (get-frame-time))
  (when (>= *lixo* 0.1)
    (when (or (is-key-down +key-a+)
              (is-key-down +key-left+))
      (decf +board-size+))
    (when (or (is-key-down +key-d+)
              (is-key-down +key-right+))
      (incf +board-size+))
    (setf *lixo* 0.0)))


(defun run ()
  (let* ((screen-width 800)
         (screen-height 600)
         (title "Bag of Peas")
         (camera-pos (make-vector3 :x 10.0 :y 10.0 :z 10.0))
         (camera-target (make-vector3 :x 0.0 :y 0.0 :z 0.0))
         (camera-up (make-vector3 :x 0.0 :y 1.0 :z 0.0))
         (camera (make-camera3d :position camera-pos
                                :target camera-target
                                :up camera-up
                                :fovy 35.0
                                :type +camera-perspective+)))
    (with-window (screen-width screen-height title)
      (set-target-fps 60)               ; Set our game to run at 60 FPS
      (loop
        (when (window-should-close) (return)) ; dectect window close button or ESC key
        (handle-input)
        (draw-all camera)))))
