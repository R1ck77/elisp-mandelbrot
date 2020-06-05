(require 'mandelbrot-math)
(require 'mandelbrot-utf-8)
(require 'cl)

(defvar mandelbrot-region '(-2 1 -1 1))
(make-variable-buffer-local 'mandelbrot-region)
(defvar mandelbrot-limits nil)
(make-variable-buffer-local 'mandelbrot-limits)

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

(defun mandelbrot/convert-coordinate (p p-max min-v max-v)
  "Transform linearly p from [1, p-max] to [min-v, max-v]"
  (float (+ (* (/ (- p 1) (- p-max 1.0)) (- (float max-v) min-v)) min-v)))

(defun mandelbrot/convert-coordinates (column row)
  (cons (mandelbrot/convert-coordinate column (car mandelbrot-limits) (car mandelbrot-region) (cadr mandelbrot-region))
        (mandelbrot/convert-coordinate row (cdr mandelbrot-limits) (elt mandelbrot-region 2) (elt mandelbrot-region 3))))

(defun mandelbrot/convert-to-map-key (value)
  "Convert the interations/nil values into elements of the advanced characters lists"
  (if value 1 0))

(defun mandelbrot/compute-quad (y y+1 row max-row)
  (let ((min-x (car mandelbrot-region))
        (max-x (cadr mandelbrot-region)))
    (let ((x (mandelbrot/convert-coordinate row max-row min-x max-x))
                (x+1 (mandelbrot/convert-coordinate (+ row 0.5) max-row min-x max-x)))
            (--map (mandelbrot/convert-to-map-key it)
                   (list (mandelbrot/insidep (cons x y))
                         (mandelbrot/insidep (cons x+1 y))
                         (mandelbrot/insidep (cons x y+1))
                         (mandelbrot/insidep (cons x+1 y+1)))))))

(defun mandelbrot/draw-quad! (y y+1 row max-row)
  (let ((result (mandelbrot/compute-quad y y+1 row max-row)))
    (insert (lax-plist-get mandelbrot-advanced-characters result))))

(defun mandelbrot/draw-4x-line! (line max-line)
  (let* ((min-y (elt mandelbrot-region 2))
         (max-y (elt mandelbrot-region 3)))
    (let ((y (mandelbrot/convert-coordinate line max-line min-y max-y))
          (y+1 (mandelbrot/convert-coordinate (+ line 0.5) max-line min-y max-y))
          (row 1))
      (while (< row (window-body-width))
        (mandelbrot/draw-quad!  y y+1 row (window-body-width))
        (setq row (1+ row))))))

(defun mandelbrot/draw-4x ()
  (goto-char (point-min))
  (message (format "The window is %d x %d" (window-body-width) (window-body-height)))
  (save-excursion
    (setq mandelbrot-limits (cons (window-body-width)
                                  (window-body-height)))
    (let ((line 1))
     (while (<= line (window-body-height))
       (mandelbrot/draw-4x-line! line (window-body-height))
       (insert "\n")
       (redisplay)
       (setq line (1+ line))))))


(provide 'mandelbrot-4x)
