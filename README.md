## Mandelbrot fractal in Emacs LISP

I'm curious to know how the Mandelbrot set would look in a text window, possibly with some cleverness in the representation (cfr the Wikipedia page for "Mandelbrot set" where the first Mandelbrot set image, as a 80x25 text image, is presented).

Also, I'm itching to carefree write some LISP code, without expectations of big projects involved.

### Status

It's horribly slow, not interactive and statically configured, but it sort of works.

If you resize your terminal and, after loading `mandelbrot-basic.el` you run this LISP bit:

    (mandelbrot/draw-simple)
    
you'll probably get something like this: 

![Mandelbrot, first draft](https://raw.githubusercontent.com/R1ck77/elisp-mandelbrot/master/images/mandelbleah.png)
    


