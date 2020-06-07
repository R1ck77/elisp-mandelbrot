(require 'buttercup)
(require 'mandelbrot-math-lisp)

(describe "mandelbrot-math-lisp"
  (describe "mandelbrot/escapedp"
    (it "returns the correct value for values inside the frontier"
      (expect (mandelbrot-lisp/escapedp '(1 . 1))
              :to-be nil))
    (it "returns the correct value for values outside the frontier"
      (expect (mandelbrot-lisp/escapedp '(100 . 10))
              :not :to-be nil))))
