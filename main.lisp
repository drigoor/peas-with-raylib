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


(defun draw-grid*2 (slices spacing &key (color +lightgray+) (border-color +gray+))
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


(defparameter *player-x* 0)
(defparameter *player-y* 0)


(defun draw-all (camera)
  (with-drawing
    (clear-background +raywhite+)
    (draw-grid*-legend +board-size+ camera)
    (with-mode-3d (camera)
      (draw-grid* +board-size+ 1.0)
      (draw-player *player-x* *player-y* +board-size+)
      (draw-mine 0 0 +board-size+))
    (draw-text "Welcome to the bag of peas" 10 40 20 +darkgray+)
    (draw-text (format nil "row: ~a" (1+ *player-x*)) 10 65 20 +darkgray+)
    (draw-text (format nil "col: ~a" (code-char (+ (char-code #\A) *player-y*))) 10 90 20 +darkgray+)
    (draw-fps 10 10)))


(defparameter *frames-counter-to-move* 0)
(defparameter *frames-counter-ro-resize* 0)


(defun handle-input ()
  (let ((can-move-p (>= *frames-counter-to-move* 15))
        (can-resize-p (>= *frames-counter-ro-resize* 15)))
    (when (and can-move-p (is-key-down +key-a+))
      (decf *player-y*)
      (setf *frames-counter-to-move* 0))
    (when (and can-move-p (is-key-down +key-d+))
      (incf *player-y*)
      (setf *frames-counter-to-move* 0))
    (when (and can-move-p (is-key-down +key-w+))
      (decf *player-x*)
      (setf *frames-counter-to-move* 0))
    (when (and can-move-p (is-key-down +key-s+))
      (incf *player-x*)
      (setf *frames-counter-to-move* 0))
    (when (and can-resize-p (is-key-down +key-left+))
      (decf +board-size+)
      (setf *frames-counter-ro-resize* 0))
    (when (and can-resize-p (is-key-down +key-right+))
      (incf +board-size+)
      (setf *frames-counter-ro-resize* 0))))


(defun run ()
  (let* ((screen-width 800)
         (screen-height 600)
         (title "Bag of Peas")
         (camera (make-camera3d :position (vec3 5 10 10)
                                :target (vec3 0 0 0)
                                :up (vec3 0 1 0)
                                :fovy 35.0
                                :type +camera-perspective+)))
    (with-window (screen-width screen-height title)
      (set-target-fps 60)               ; Set our game to run at 60 FPS
      (setf *frames-counter-to-move* 0)
      (setf *frames-counter-ro-resize* 0)
      (loop
        (when (window-should-close) (return)) ; dectect window close button or ESC key
        (handle-input)
        (draw-all camera)
        (incf *frames-counter-to-move*)
        (incf *frames-counter-ro-resize*)))))
