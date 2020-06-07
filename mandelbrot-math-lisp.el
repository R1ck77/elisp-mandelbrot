(require 'dash)
(require 'anaphora)

;;; TOFO/FIXME extract
(defconst mandelbrot-boundary 4) ; if |z|^2 > mandelbrot-boundary the value is considered outside the set

(defun mandelbrot-lisp/+ (a b)
  (cons (float (+ (car a) (car b)))
        (float (+ (cdr a) (cdr b)))))

(defun mandelbrot/* (a b)
  (let ((ar (car a))
        (ai (cdr a))
        (br (car b))
        (bi (cdr b)))
    (cons (float
           (- (* ar br)
              (* ai bi)))
          (float
           (+ (* ar bi)
              (* br ai))))))

(defun mandelbrot-lisp/sqr (a)
  (mandelbrot/* a a))

(defun mandelbrot-lisp/f (z c)
  (mandelbrot-lisp/+ (mandelbrot-lisp/sqr z) c))

(defun mandelbrot-lisp/modulo-squared (v)
  (float
   (+ (* (car v) (car v))
      (* (cdr v) (cdr v)))))

(defun mandelbrot-lisp/escapedp (v)
  (> (mandelbrot/modulo-squared v) mandelbrot-boundary))

(defun mandelbrot-lisp/compute-trajectory (c maximum-iterations)
  "Returns nil if the point doesn't escape in 'iterations', or the number of iterations it takes to escape otherwise

There may be an (harmless) off by one on the number of iterations returned (not clear what the standard is on the matter)."
  (let ((escaped)
        (iterations 0)
        (z '(0 . 0)))
    (while (and (< iterations maximum-iterations) (not escaped))
      (let ((next-value (mandelbrot/f z c)))
        (if (mandelbrot-lisp/escapedp next-value)
            (setq escaped iterations)
          (progn
            (setq z next-value)
            (setq iterations (1+ iterations))))))
    escaped))

(defun mandelbrot-lisp/insidep (c iterations)
  (not (mandelbrot-lisp/compute-trajectory c iterations)))





(provide 'mandelbrot-math-lisp)
