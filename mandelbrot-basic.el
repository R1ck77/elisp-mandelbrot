(require 'mandelbrot)

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

;; TODO/FIXME test this as an exercise
(defun mandelbrot/draw-simple ()
  (mandelbrot/clear-buffer! mandelbrot-basic-name)
  (goto-char (point-min))
  (message (format "The window is %d x %d" (window-body-width) (window-body-height)))
  
  )


(provide 'mandelbrot-basic)
