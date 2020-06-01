(require 'mandelbrot)
(require 'mandelbrot-utf-8)
(require 'cl)

(defconst min-x -2)
(defconst max-x 1)
(defconst min-y -1)
(defconst max-y 1)

(defvar mandelbrot-advanced-characters
  `((0 0 0 0) ,mandelbrot-empty
    (1 0 0 0) ,mandelbrot-quadrant-upper-left
    (0 1 0 0) ,mandelbrot-quadrant-upper-right
    (1 1 0 0) ,mandelbrot-upper-half-block
    (0 0 1 0) ,mandelbrot-quadrant-lower-left
    (1 0 1 0) ,mandelbrot-left-half-block
    (0 1 1 0) ,mandelbrot-quadrant-upper-right-and-lower-left
    (1 1 1 0) ,mandelbrot-quadrant-upper-left-and-upper-right-and-lower-left
    (0 0 0 1) ,mandelbrot-quadrant-lower-right
    (1 0 0 1) ,mandelbrot-quadrant-upper-left-and-lower-right
    (0 1 0 1) ,mandelbrot-right-half-block
    (1 1 0 1) ,mandelbrot-quadrant-upper-left-and-upper-right-and-lower-right
    (0 0 1 1) ,mandelbrot-lower-half-block
    (1 0 1 1) ,mandelbrot-quadrant-upper-left-and-lower-left-and-lower-right
    (0 1 1 1) ,mandelbrot-quadrant-upper-right-and-lower-left-and-lower-right
    (1 1 1 1) ,mandelbrot-full-block))

(defconst mandelbrot-advanced-name "*Mandelbrot*")

(defun mandelbrot/clear-buffer! (name)
  (let ((buffer (get-buffer-create name)))
    (switch-to-buffer buffer)
    (erase-buffer)
    buffer))

(defun mandelbrot/convert-coordinate (p p-max min-v max-v)
  "Transform linearly p from [1, p-max] to [min-v, max-v]"
  (float (+ (* (/ (- p 1) (- p-max 1.0)) (- (float max-v) min-v)) min-v)))

(defun mandelbrot/draw-point! (y row max-row)
  (let ((x (mandelbrot/convert-coordinate row max-row min-x max-x)))
    (insert (if (mandelbrot/insidep (cons x y)) mandelbrot-basic-full mandelbrot-basic-empty))))

(defun mandelbrot/draw-line! (line max-line)
  (let ((y (mandelbrot/convert-coordinate line max-line min-y max-y))
        (row 1))
    (while (< row (window-body-width))
      (mandelbrot/draw-point! y row (window-body-width))
      (redisplay)
      (setq row (1+ row)))))

;; TODO/FIXME test this as an exercise
(defun mandelbrot/draw-simple ()
  (mandelbrot/clear-buffer! mandelbrot-advanced-name)
  (goto-char (point-min))
  (message (format "The window is %d x %d" (window-body-width) (window-body-height)))
  (let ((line 1))
    (while (< line (window-body-height))
      (mandelbrot/draw-line! line (window-body-height))
      (insert "\n")
      (setq line (1+ line)))))


(provide 'mandelbrot-basic)
