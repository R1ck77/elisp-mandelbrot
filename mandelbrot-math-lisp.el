(require 'dash)
(require 'anaphora)

;;; TOFO/FIXME extract
(defconst mandelbrot-boundary 4) ; if |z|^2 > mandelbrot-boundary the value is considered outside the set

(defun mandelbrot-lisp/+ (a b)
  (cons (float (+ (car a) (car b)))
        (float (+ (cdr a) (cdr b)))))

(provide 'mandelbrot-math-lisp)
