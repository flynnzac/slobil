ifndef PREFIX
	PREFIX=/usr/local/
endif

ifeq ($(OS),Windows_NT)
	SUFFIX=dll
else
	SUFFIX=so
endif

arbel: src/*.c src/arbel.h pushmac
	make -C src GARBAGE=$(GARBAGE)

pushmac:
	make -C pushmac
	make -C pushmac install

doc: docs/arbel.html docs/index.mro.html docs/examples.mro.html
	cat docs/mysite.mro docs/index.mro.html | pushmac > docs/index.html
	cat docs/mysite.mro docs/examples.mro.html | pushmac > docs/examples.html

examples: examples/link.c libarbel.$(SUFFIX) .FORCE
	cc -c -fPIC -I. -L. -larbel -lm examples/link.c
	cc -shared -fPIC -o link.$(SUFFIX) -L. -I. link.o -larbel

install: arbel libarbel.$(SUFFIX) src/arbel.h
	cp arbel $(PREFIX)/bin/
	cp libarbel.$(SUFFIX) $(PREFIX)/lib/
	cp src/arbel.h $(PREFIX)/include/

