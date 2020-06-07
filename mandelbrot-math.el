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

(fset 'mandelbrot/modulo-squared (if mandelbrot-binary-library-loaded
                                     #'mandelbrot-c/modulo-squared
                                   #'mandelbrot-lisp/modulo-squared))

(defun mandelbrot/insidep-c-wrapper (c iterations)
  (= (mandelbrot-c/compute-trajectory c iterations) -1))

(fset 'mandelbrot/insidep (if mandelbrot-binary-library-loaded
                              #'mandelbrot/insidep-c-wrapper
                            #'mandelbrot-lisp/insidep))

(provide 'mandelbrot-math)
