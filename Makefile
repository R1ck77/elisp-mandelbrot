.PHONY: test clean all timing

EMACS_INCLUDE_FOLDER=-I${HOME}/local/emacs/include
CFLAGS=-Wall -Wextra -O3 -fPIC $(EMACS_INCLUDE_FOLDER)

test:
	emacs --eval "(setq load-path (cons \".\" load-path))" -batch -f package-initialize -f buttercup-run-discover

all: libmandelbrot.so

libmandelbrot.so: mandelbrot.c
	gcc -shared $(CFLAGS) -o $@ $<

timing:
	/usr/bin/time emacs -nw -f package-initialize --eval "(progn (setq load-path (cons \".\" load-path)) (require 'mandelbrot) (mandelbrot-mode) (kill-emacs))"

clean:
	rm -f libmandelbrot.so

