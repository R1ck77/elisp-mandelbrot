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
              :to-equal '(5 . 12)))))
