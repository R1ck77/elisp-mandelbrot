(require 'mandelbrot-math-lisp)

(defconst mandelbrot-boundary 4) ; if |z|^2 > mandelbrot-boundary the value is considered outside the set

(defvar mandelbrot-binary-library-loaded nil
  "This variable is t if the binary library is compiled and can be loaded")


(defun mandelbrot/load--library ()
  (condition-case nil
      (setq mandelbrot-binary-library-loaded (load-library "./libmandelbrot.so"))
    ('error nil)))
(mandelbrot/load--library)

(fset 'mandelbrot/+ (if mandelbrot-binary-library-loaded
                        #'mandelbrot-c/+
                      #'mandelbrot-lisp/+))

(fset 'mandelbrot/sqr (if mandelbrot-binary-library-loaded
                        #'mandelbrot-c/sqr
                      #'mandelbrot-lisp/sqr))

;;; This fset overrides the previous two
(fset 'mandelbrot/f (if mandelbrot-binary-library-loaded
                        #'mandelbrot-c/f
                      #'mandelbrot-lisp/f))

(defun mandelbrot/modulo-squared (v)
  (float
   (+ (* (car v) (car v))
      (* (cdr v) (cdr v)))))

(defun mandelbrot/escapedp (v)
  (> (mandelbrot/modulo-squared v) mandelbrot-boundary))

(defun mandelbrot/compute-trajectory (c maximum-iterations)
  "Returns nil if the point doesn't escape in 'iterations', or the number of iterations it takes to escape otherwise

There may be an (harmless) off by one on the number of iterations returned (not clear what the standard is on the matter)."
  (let ((escaped)
        (iterations 0)
        (z '(0 . 0)))
    (while (and (< iterations maximum-iterations) (not escaped))
      (let ((next-value (mandelbrot/f z c)))
        (if (mandelbrot/escapedp next-value)
            (setq escaped iterations)
          (progn
            (setq z next-value)
            (setq iterations (1+ iterations))))))
    escaped))

(defun mandelbrot/insidep (c iterations)
  (not (mandelbrot/compute-trajectory c iterations)))

(provide 'mandelbrot-math)
