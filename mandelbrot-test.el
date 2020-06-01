(require 'buttercup)
(require 'mandelbrot)

(describe "mandelbrot"
  (describe "mandelbrot/+"
    (it "sums two complex numbers correctly"
      (expect (mandelbrot/+ '(1 . 2) '(4 . 5))
              :to-equal '(5 . 7))))
  (describe "mandelbrot/*"
    (it "multiplies a complex number by a real constant correctly"
      (expect (mandelbrot/* '(1 . 3) '(4 . 0))
              :to-equal '(4 . 12)))
    (it "multiplies two complex numbers correctly"
      (expect (mandelbrot/* '(1 . 3) '(2 . 7))
              :to-equal '(-19 . 13))))
  (describe "mandelbrot/sqr"
    (it "squares a complex nubmer"
      (expect (mandelbrot/sqr '(3 . 2))
              :to-equal '(5 . 12))))
  (describe "mandelbrot/f"
    (it "computes the z=0 case correctly"
      (expect (mandelbrot/f '(0 . 0) '(3 . 2))
              :to-equal '(3 . 2)))
    (it "computes the c=0 case correctly"
      (expect (mandelbrot/f '(3 . 2) '(0 . 0))
              :to-equal '(5 . 12))))
  (describe "mandelbrot/modulo-squared"
    (it "computes the square of the length of the complex vector associated with a point"
      (expect (mandelbrot/modulo-squared '(3 . 4))
              :to-be 25)))
  (describe "mandelbrot/escapedp"
    (it "returns the correct value for values inside the frontier"
      (expect (mandelbrot/escapedp '(1 . 1))
              :to-be nil))
    (it "returns the correct value for values outside the frontier"
      (expect (mandelbrot/escapedp '(100 . 10))
              :not :to-be nil)))
  (describe "mandelbrot/insidep"
    (it "returns true for c = 0"
      (expect (mandelbrot/insidep '(0 . 0))
              :not :to-be nil))
    (it "returns false for points outside the disk of radius 2"
      (expect (mandelbrot/insidep '(2.1 . 0)) :to-be nil)
      (expect (mandelbrot/insidep '(0 . 2.1)) :to-be nil)
      (expect (mandelbrot/insidep '(-2.1 . 0)) :to-be nil)
      (expect (mandelbrot/insidep '(0 . -2.1)) :to-be nil))
    (it "is true for some special cases"
      (expect (mandelbrot/insidep '(-1 . 0)) :not :to-be nil)
      (expect (mandelbrot/insidep '(0 . 0.5)) :not :to-be nil)
      (expect (mandelbrot/insidep '(0 . -0.5)) :not :to-be nil))))
