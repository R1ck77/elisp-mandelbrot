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




(provide 'mandelbrot-math-lisp)
