## Mandelbrot fractal in Emacs LISP

I'm curious to know how the Mandelbrot set would look in a text window, possibly with some cleverness in the representation (cfr the Wikipedia page for "Mandelbrot set" where the first Mandelbrot set image, as a 80x25 text image, is presented).

Also, I'm itching to write some LISP code carefree, without expectations or any big projects involved.

### Status

If you load and evaluate `mandelbrot.el` and then execute

    M-x mandelbrot-mode
    
you'll probably get something like this: 

![Mandelbrot, second  draft](https://raw.githubusercontent.com/R1ck77/elisp-mandelbrot/master/images/mehndelbrot.png)

but your mileage will vary, depending on the fonts and terminal used.

The terminal version of Emacs does a pretty decent job, where the windowed one looks awful-er.

### Limitations

It's a Mandelbrot program that uses ELISP for computations and draws the result in a text windows, how many more limitations do you want? xD

The code doesn't employ any neat trick (like symmetry exploitation) to make the calculations faster.

### TODO

I'm not going to over-engineer this thing, but I still hope to add a couple of commands to zoom on the set and change the number of computations.
    

    


