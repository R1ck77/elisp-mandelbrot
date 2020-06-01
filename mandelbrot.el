(require 'dash)
(require 'anaphora)

(defconst mandelbrot-boundary 1e3) ; if |z|^2 > mandelbrot-boundary the value is considered outside the set
(defconst mandelbrot-iterations 100)

(defun mandelbrot/+ (a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(defun mandelbrot/* (a b)
  (let ((ar (car a))
        (ai (cdr a))
        (br (car b))
        (bi (cdr b)))
    (cons (- (* ar br)
             (* ai bi))
          (+ (* ar bi)
             (* br ai)))))

(defun mandelbrot/sqr (a)
  (mandelbrot/* a a))

(defun mandelbrot/f (z c)
  (mandelbrot/+ (mandelbrot/sqr z) c))

(defun mandelbrot/modulo-squared (v)
  (+ (* (car v) (car v))
     (* (cdr v) (cdr v))))

(defun mandelbrot/escapedp (v)
  (> (mandelbrot/modulo-squared v) mandelbrot-boundary))

(defun mandelbrot/compute-trajectory (c z iterations)
  "Returns nil if the point doesn't escape in 'mandelbrot-iterations', or the number of iterations it takes to escape otherwise

There may be an (harmless) off by one on the number of iterations returned (not clear what the standard is on the matter)."
  (let ((next-value (mandelbrot/f z c)))
    (if (mandelbrot/escapedp next-value)
        iterations
      (if (< iterations mandelbrot-iterations)
          (mandelbrot/compute-trajectory c next-value (1+ iterations))
        nil))))

(defun mandelbrot/insidep (c)
  (not (mandelbrot/compute-trajectory c '(0 . 0) 0)))

(provide 'mandelbrot)
