## Mandelbrot fractal in Emacs LISP

## Purpose

I was curious to know how the Mandelbrot set would look in a text window, possibly with some cleverness in the representation.

Along the way I used this to explore the benefits of Emacs dynamic modules and practice a bit with them, which I did, with some satisfaction.

Last but not least, I constantly itch to do some carefree LISP coding, without big projects or responsibilities involved.

### Installation

You need to make sure that all files can be loaded when you evaluate `mandelbrot.el` (I don't have much experience with ELISP development. Is this obvious?).

I usually evaluate something like this:

    M-x eval-expression
    Eval: (setq load-path (cons "." load-path))
    
and then, with `mandelbrot.el` opened:

    M-x eval-buffer
    
There is an *optional* support C library to speed the Mandelbrot computation  **a lot** (I didn't time the speed-up, but depending on the number of iterations and on the region selected you can get improvements up and above a 25x factor).

If you don't get or can't compile the C library, the LISP drop-in replacement is slower but works just fine.

On Linux, make sure your emacs supports dynamic modules (the `module-file-suffix` variable should not be `nil`) and then change the value of the `EMACS_INCLUDE_FOLDER` variable in the `Makefile` to point to the include folder with the `emacs-module.h` file.

The `Makefile` uses `GCC`, but if you are on Linux, I'm sure it won't be a problem.

Run:

    $ make all
    
to compile.

### Usage

Run

    M-x mandelbrot-mode
    
and if your terminal has characters tiny enough, you'll probably get something like this: 

![Mandelbrot, second  draft](https://raw.githubusercontent.com/R1ck77/elisp-mandelbrot/master/images/mehndelbrot.png)

Your mileage will vary, depending on the fonts and terminal used.

The non-graphic version of Emacs does a pretty decent job, where the windowed one may look awful-er due to possible space between characters.

The major mode has a number of simple commands:

- to zoom on a region (C-space to mark the start of the region, then z)
- change the maximum number of iterations (i)
- redraw (r)
- reset to the initial region (R).

### Bugs and limitations

Many. It's just an exploratory exercise after all.

The code doesn't employ any neat trick (like symmetry exploitation) to make the calculations faster and it's not really optimized, also one could push the C integration much further to employ multi-threaded or SIMD computing etc, but it was never my purpose.


    


