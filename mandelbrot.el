(require 'mandelbrot-4x)

(defconst mandelbrot-buffer-name "*Mandelbrot*")

(defvar mandelbrot-mode-hook nil
  "*List of functions to call when entering Mandelbrot mode.*")

(defvar mandelbrot-mode-map nil
  "Keymap for Mandelbrot mode")
(when (not mandelbrot-mode-map)
  (setq mandelbrot-mode-map (make-keymap))
  ;;;  (define-key )
  )

(defun mandelbrot/configure-canvas ()
  (let ((buffer (get-buffer-create mandelbrot-buffer-name)))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    buffer))

(defun mandelbrot/redraw ()
  "Redraw mandelbrot in the current buffer"
  (let ((buffer-read-only nil))
    (mandelbrot/draw-4x)))

(defun mandelbrot-mode ()
  "Enter Mandelbrot mode"
  (interactive)
  (mandelbrot/configure-canvas)
  (kill-all-local-variables)
  (setq major-mode 'mandelbrot-mode)
  (setq mode-name "Mbrot")
  (use-local-map mandelbrot-mode-map)
  (run-mode-hooks 'mandelbrot-mode-hook)
  (mandelbrot/redraw))

(provide 'mandelbrot)
