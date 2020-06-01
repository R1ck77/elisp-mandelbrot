(require 'mandelbrot)
(require 'cl)

(defconst min-x -2)
(defconst max-x 1)
(defconst min-y -1)
(defconst max-y 1)

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
