(require 'mandelbrot-4x)

(defconst mandelbrot-buffer-name "*Mandelbrot*")

(defvar mandelbrot-mode-hook nil
  "*List of functions to call when entering Mandelbrot mode.*")

(defvar mandelbrot-mode-map nil
  "Keymap for Mandelbrot mode")
(when (not mandelbrot-mode-map)
  (setq mandelbrot-mode-map (make-keymap))
  (define-key mandelbrot-mode-map (kbd "r") #'mandelbrot/redraw)
  (define-key mandelbrot-mode-map (kbd "p") #'mandelbrot/print-coordinate))

(defmacro mandelbrot/with-read-only-disabled (&rest forms)
  `(let ((buffer-read-only nil))
     ,@forms))

(defun mandelbrot/configure-canvas ()
  (let ((buffer (get-buffer-create mandelbrot-buffer-name)))
    (switch-to-buffer buffer)
    (mandelbrot/with-read-only-disabled 
     (erase-buffer))
    buffer))

(defun mandelbrot/redraw ()
  (interactive)
  "Redraw mandelbrot in the current buffer"
  (mandelbrot/with-read-only-disabled   
   (erase-buffer)
   (mandelbrot/draw-4x)))

(defun mandelbrot/print-coordinate ()
  (interactive)
  "Print the coordinate at point"
  (message "Not yet implemented"))

(defun mandelbrot-mode ()
  "Enter Mandelbrot mode"
  (interactive)
  (mandelbrot/configure-canvas)
  (kill-all-local-variables)
  (setq major-mode 'mandelbrot-mode)
  (setq mode-name "Mbrot")
  (use-local-map mandelbrot-mode-map)
  (run-mode-hooks 'mandelbrot-mode-hook)
  (setq buffer-read-only t)
  (mandelbrot/redraw))

(provide 'mandelbrot)
