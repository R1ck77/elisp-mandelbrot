.PHONY: test clean all

EMACS_INCLUDE_FOLDER=-I${HOME}/local/emacs/include
CFLAGS=-Wall -Wextra -O3 -fPIC $(EMACS_INCLUDE_FOLDER)

test:
	emacs --eval "(setq load-path (cons \".\" load-path))" -batch -f package-initialize -f buttercup-run-discover

all: libmandelbrot.so

libmandelbrot.so: mandelbrot.c
	gcc -shared $(CFLAGS) -o $@ $<

clean:
	rm -f libmandelbrot.so

