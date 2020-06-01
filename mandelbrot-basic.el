(require 'mandelbrot)
(require 'cl)

(defconst min-x -2)
(defconst max-x 1)
(defconst min-y -1)
(defconst max-y 1)

(defvar empty " ")
(defvar quadrant-upper-left "▘")
(defvar quadrant-upper-right "▝")
(defvar upper-half-block "▀")
(defvar quadrant-lower-left  "▖")
(defvar left-half-block "▌")
(defvar quadrant-upper-right-and-lower-left "▞")
(defvar quadrant-upper-left-and-upper-right-and-lower-left "▛")
(defvar quadrant-lower-right "▗")
(defvar quadrant-upper-left-and-lower-right "▚")
(defvar right-half-block "▐")
(defvar quadrant-upper-left-and-upper-right-and-lower-right "▜")
(defvar lower-half-block "▄")
(defvar quadrant-upper-left-and-lower-left-and-lower-right "▙")
(defvar quadrant-upper-right-and-lower-left-and-lower-right "▟")
(defvar full-block "█")

(defvar advanced '((0 0 0 0) empty
                   (1 0 0 0) quadrant-upper-left
                   (0 1 0 0) quadrant-upper-right
                   (1 1 0 0) upper-half-block
                   (0 0 1 0) quadrant-lower-left
                   (1 0 1 0) left-half-block
                   (0 1 1 0) quadrant-upper-right-and-lower-left
                   (1 1 1 0) quadrant-upper-left-and-upper-right-and-lower-left
                   (0 0 0 1) quadrant-lower-right
                   (1 0 0 1) quadrant-upper-left-and-lower-right
                   (0 1 0 1) right-half-block
                   (1 1 0 1) quadrant-upper-left-and-upper-right-and-lower-right
                   (0 0 1 1) lower-half-block
                   (1 0 1 1) quadrant-upper-left-and-lower-left-and-lower-right
                   (0 1 1 1) quadrant-upper-right-and-lower-left-and-lower-right
                   (1 1 1 1) full-block))

(defconst mandelbrot-basic-name "*Mandelbrot*")

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
    (insert (if (mandelbrot/insidep (cons x y)) "x" "."))))

(defun mandelbrot/draw-line! (line max-line)
  (let ((y (mandelbrot/convert-coordinate line max-line min-y max-y))
        (row 1))
    (while (< row (window-body-width))
      (mandelbrot/draw-point! y row (window-body-width))
      (redisplay)
      (setq row (1+ row)))))

;; TODO/FIXME test this as an exercise
(defun mandelbrot/draw-simple ()
  (mandelbrot/clear-buffer! mandelbrot-basic-name)
  (goto-char (point-min))
  (message (format "The window is %d x %d" (window-body-width) (window-body-height)))
  (let ((line 1))
    (while (< line (window-body-height))
      (mandelbrot/draw-line! line (window-body-height))
      (insert "\n")
      (setq line (1+ line)))))


(provide 'mandelbrot-basic)
