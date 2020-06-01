(require 'dash)
(require 'anaphora)

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






(provide 'mandelbrot)
