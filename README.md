## Mandelbrot fractal in Emacs LISP

I'm curious to know how the Mandelbrot set would look in a text window, possibly with some cleverness in the representation (cfr the Wikipedia page for "Mandelbrot set" where the first Mandelbrot set image, as a 80x25 text image, is presented).

Also, I'm itching to write some LISP code carefree, without expectations or any big projects involved.

### Status

If you load and evaluate `mandelbrot.el` and then execute

    M-x mandelbrot-mode
    
you'll probably get something like this: 

![Mandelbrot, second  draft](https://raw.githubusercontent.com/R1ck77/elisp-mandelbrot/master/images/mehndelbrot.png)

but your mileage will vary, depending on the fonts and terminal used.

The terminal version of Emacs does a pretty decent job, where the windowed one looks awful-er due to characters spacing.

The major mode has a number of simple commands to zoom on a region (C-space to mark the start of the region, then z), change the maximum number of iterations (i), redraw (r) or reset to the initial region (R).

### Limitations

It's a Mandelbrot program that uses ELISP for computations and draws the result in a text windows, how many more limitations do you want? xD

The code doesn't employ any neat trick (like symmetry exploitation) to make the calculations faster.

### TODO

I'd like to toy with dynamic modules and provide a C-like quick mandelbrot alternative. Also, I'd like to explore different levels of integration with C, from the lowest level (which I expect to provide no benefit) to the highest:

- C for Complex addition and multiplication only
- C for mandelbrot computations
- C for trajectory computation

where I expect the first solution to be slower, the second to be around the break-even point and the third to be faster.

I'd like to mark the region the user is zooming in with color, also the zoom command is a bit bugged, as it allows to flip the graph (tolerated atm, but not intentional…).

Showing the current maximum number of iterations during query and offering the current value as default may also be nice.


    


