(require 'buttercup)
(require 'mandelbrot-math)

(describe "mandelbrot-math"
  (describe "mandelbrot/+"
    (it "sums two complex numbers correctly"
      (expect (mandelbrot/+ '(1 . 2) '(4 . 5))
              :to-equal '(5.0 . 7.0))))
  (describe "mandelbrot/*"
    (it "multiplies a complex number by a real constant correctly"
      (expect (mandelbrot/* '(1 . 3) '(4 . 0))
              :to-equal '(4.0 . 12.0)))
    (it "multiplies two complex numbers correctly"
      (expect (mandelbrot/* '(1 . 3) '(2 . 7))
              :to-equal '(-19.0 . 13.0))))
  (describe "mandelbrot/sqr"
    (it "squares a complex nubmer"
      (expect (mandelbrot/sqr '(3 . 2))
              :to-equal '(5.0 . 12.0))))
  (describe "mandelbrot/f"
    (it "computes the z=0 case correctly"
      (expect (mandelbrot/f '(0 . 0) '(3 . 2))
              :to-equal '(3.0 . 2.0)))
    (it "computes the c=0 case correctly"
      (expect (mandelbrot/f '(3 . 2) '(0 . 0))
              :to-equal '(5.0 . 12.0))))
  (describe "mandelbrot/modulo-squared"
    (it "computes the square of the length of the complex vector associated with a point"
      (expect (mandelbrot/modulo-squared '(3 . 4))
              :to-equal 25.0)))
  (describe "mandelbrot/insidep"
    (it "returns true for c = 0"
      (expect (mandelbrot/insidep '(0 . 0) 100)
              :not :to-be nil))
    (it "returns false for points outside the disk of radius 2"
      (expect (mandelbrot/insidep '(2.1 . 0) 100) :to-be nil)
      (expect (mandelbrot/insidep '(0 . 2.1) 100) :to-be nil)
      (expect (mandelbrot/insidep '(-2.1 . 0) 100) :to-be nil)
      (expect (mandelbrot/insidep '(0 . -2.1) 100) :to-be nil))
    (it "is true for some special cases"
      (expect (mandelbrot/insidep '(-1 . 0) 100) :not :to-be nil)
      (expect (mandelbrot/insidep '(0 . 0.5) 100) :not :to-be nil)
      (expect (mandelbrot/insidep '(0 . -0.5) 100) :not :to-be nil))))
