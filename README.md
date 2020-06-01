## Mandelbrot fractal in Emacs LISP

I'm curious to know how the Mandelbrot set would look in a text window, possibly with some cleverness in the representation (cfr the Wikipedia page for "Mandelbrot set" where the first Mandelbrot set image, as a 80x25 text image, is presented).

Also, I'm itching to carefree write some LISP code, without expectations of big projects involved.

### Status

It (purposely) doesn't exploit vertical simmetry, it's quite slow and it always render the same region, but it sort of works and uses utf-8 characters to increase the resolution.

If you resize your terminal and, after loading `mandelbrot-4x.el` you run this LISP bit:

    (mandelbrot/draw-4x)
    
you'll probably get something like this: 

![Mandelbrot, second  draft](https://raw.githubusercontent.com/R1ck77/elisp-mandelbrot/master/images/mehndelbrot.png)
    


