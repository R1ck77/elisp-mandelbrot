(require 'mandelbrot-4x)

(defconst mandelbrot-buffer-name "*Mandelbrot*")

(defvar mandelbrot-mode-hook nil
  "*List of functions to call when entering Mandelbrot mode.*")

(defvar mandelbrot-mode-map nil
  "Keymap for Mandelbrot mode")
(when (not mandelbrot-mode-map)
  (setq mandelbrot-mode-map (make-keymap))
  (define-key mandelbrot-mode-map (kbd "r") #'mandelbrot/redraw)
  (define-key mandelbrot-mode-map (kbd "R") #'mandelbrot/reset)
  (define-key mandelbrot-mode-map (kbd "p") #'mandelbrot/print-coordinate)
  (define-key mandelbrot-mode-map (kbd "C-@") #'mandelbrot/mark-start)
  (define-key mandelbrot-mode-map (kbd "z") #'mandelbrot/zoom)
  (define-key mandelbrot-mode-map (kbd "i") #'mandelbrot/change-iterations))

(defmacro mandelbrot/with-read-only-disabled (&rest forms)
  `(let ((buffer-read-only nil))
     ,@forms))

(defun mandelbrot/configure-canvas ()
  (let ((buffer (get-buffer-create mandelbrot-buffer-name)))
    (switch-to-buffer buffer)
    (mandelbrot/with-read-only-disabled 
     (erase-buffer))
    buffer))

(defun mandelbrot/reset ()
  "Reset the plot to the initial region"
  (interactive)
  (setq mandelbrot-region mandelbrot-initial-boundaries)
  (mandelbrot/redraw))

(defun mandelbrot/redraw ()
  "Redraw mandelbrot in the current buffer"  
  (interactive)
  (mandelbrot/with-read-only-disabled   
   (erase-buffer)
   (mandelbrot/draw-4x)))

;;; TODO/FIXME this is a mess
(defvar mandelbrot-start-position)
(make-variable-buffer-local 'mandelbrot-start-position)
(defun mandelbrot/mark-start ()
  "Set the starting point for a zoom area"
  (interactive)
  (setq mandelbrot-start-position (and (not mandelbrot-start-position)
                                       (point))))

(defun mandelbrot/get-current-x-y ()
    (let ((row (line-number-at-pos))
         (column (1+ (- (point) (line-beginning-position)))))
      (mandelbrot/column-row-to-x-y column row)))

(defun mandelbrot/print-coordinate ()
  "Print the coordinate at point"
  (interactive)
  (let ((coordinates (mandelbrot/get-current-x-y)))
    (message "The position is: %s,%s" (car coordinates) (cdr coordinates))))

(defun mandelbrot/rectify-region (x1 x2 y1 y2)
  (let ((x-coords (if (< x1 x2) (list x1 x2) (list x2 x1)))
        (y-coords (if (< y1 y2) (list y1 y2) (list y2 y1))))
    (append x-coords y-coords)))

(defun mandelbrot/zoom ()
  "Zoom in on the region"
  (interactive)
  (save-excursion
    ;;; TODO/FIXME reorder points!
    (let ((start-position)
          (end-position))
      (setq end-position (mandelbrot/get-current-x-y))
      (goto-char mandelbrot-start-position)
      (setq start-position (mandelbrot/get-current-x-y))
      (setq mandelbrot-region (mandelbrot/rectify-region (car start-position) (car end-position)
                                                         (cdr start-position) (cdr end-position)))
      (message "Zooming to: %s" mandelbrot-region)
      (mandelbrot/redraw))))

(defun mandelbrot/change-iterations (maximum-iterations)
  "Update the number of iterations"
  (interactive "NIterations: ")
  (setq mandelbrot-iterations maximum-iterations)
  (mandelbrot/redraw))

(defun mandelbrot/clear-background-color ()
  (setq buffer-read-only nil)
  (put-text-property (point-min) (point-max)
                     'font-lock-face (list :background "clear"))
  (setq buffer-read-only t))

(defun mandelbrot/mark-line ()
  )

(defun mandelbrot/point-to-coords (point)
  "Return a cons list with column and line")

(defun mandelbrot/sort-coordinates (point-a point-b)
  (let ((coords-a (mandelbrot/point-to-coords point-a))
        (coords-b (mandelbrot/point-to-coords point-b)))
    (list (cons (min (car coords-a) (car coords-b))
                (min (cdr coords-a) (cdr coords-b)))
          (cons (max (car coords-a) (car coords-b))
                (max (cdr coords-a) (cdr coords-b))))))

(defun mandelbro/mark-selection ()
  (save-excursion
    (apply #'mandelbrot/mark-rectangle
           (mandelbrot/sort-coordinates (point)
                                        mandelbrot/mark-selection))))

(defun mandelbrot/mark-region-hook ()
  "Used to highlight the marked region. Very crude ATM."
  (if mandelbrot-start-position
      (mandelbrot/mark-selection)
    (mandelbrot/clear-background-color)))
  

(defun mandelbrot-mode ()
  "Enter Mandelbrot mode

\\{mandelbrot-mode-map}"
  (interactive)
  (mandelbrot/configure-canvas)
  (kill-all-local-variables)
  (setq major-mode 'mandelbrot-mode)
  (setq mode-name "Mbrot")
  (use-local-map mandelbrot-mode-map)
  (run-mode-hooks 'mandelbrot-mode-hook)
  (add-hook 'post-command-hook #'mandelbrot/mark-region-hook t t)
  (setq buffer-read-only t)
  (mandelbrot/redraw))

(provide 'mandelbrot)
