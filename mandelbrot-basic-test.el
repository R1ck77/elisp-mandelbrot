(require 'buttercup)
(require 'mandelbrot-basic)

(describe "mandelbrot-basic"
  (describe "mandelbrot-convert-coordinate"
    (it "converts 1 in [1:12] to [3:7] correctly"
      (expect (mandelbrot/convert-coordinate 1 12 3 7)
              :to-be-close-to 3 0))
    (it "converts 12 in [1:12] to [3:7] correctly"
      (expect (mandelbrot/convert-coordinate 12 12 3 7)
              :to-be-close-to 7 0))
    (it "converts 4 in [1:5] to [3:7] correctly"
      (expect (mandelbrot/convert-coordinate 4 5 3 7)
              :to-be-close-to 6 0))))
