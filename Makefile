ifndef PREFIX
	PREFIX=/usr/local/
endif

ifeq ($(OS),Windows_NT)
	SUFFIX=dll
else
	SUFFIX=so
endif

.FORCE:

arbel: .FORCE
	make -C src

doc: docs/arbel.html docs/index.mro.html docs/examples.mro.html
	cat docs/mysite.mro docs/index.mro.html | mro > docs/index.html
	cat docs/mysite.mro docs/examples.mro.html | mro > docs/examples.html

examples: examples/link.c libarbel.$(SUFFIX) .FORCE
	cc -c -fPIC -I. -L. -larbel -lm examples/link.c
	cc -shared -fPIC -o link.$(SUFFIX) -L. -I. link.o -larbel

install: arbel libarbel.$(SUFFIX) src/arbel.h
	cp arbel $(PREFIX)/bin/
	cp libarbel.$(SUFFIX) $(PREFIX)/lib/
	cp src/arbel.h $(PREFIX)/include/

