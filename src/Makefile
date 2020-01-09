ifndef PREFIX
	PREFIX=/usr/local/
endif

CFLAGS_USE = -O3 -lm -Wall -ldl -lunistring -lreadline $(CFLAGS)
COMPILE=cc -o ../arbel arbel.c utility.c save.c parse.c statement.c compute.c copy.c memory.c print.c register.c registry.c state.c type.c operator.c arg.c data.c $(CFLAGS_USE)

ifeq ($(OS),Windows_NT)
	SUFFIX=dll
else
	SUFFIX=so
endif



arbel: arbel.c operator.c utility.c parse.c statement.c save.c compute.c copy.c memory.c print.c register.c registry.c state.c type.c arg.c 
	cc -B src -c -fPIC register.c registry.c state.c memory.c copy.c compute.c utility.c save.c statement.c parse.c type.c arg.c print.c arg.c data.c -O3 -lm -Wall -ldl -lreadline
	cc -B src -o ../libarbel.$(SUFFIX) -fPIC -shared register.o registry.o state.o memory.o copy.o compute.o utility.o save.o statement.o parse.o type.o print.o arg.o data.o
	$(COMPILE)

operator.c: operator.mro.c
	cat operator.mro.c | pushmac > operator.c

doc: docs/arbel.dbk docs/arbel.css
	$(DOC)
	cat docs/mysite.mro docs/index.mro.html | pushmac > docs/index.html

examples: examples/link.c libarbel.$(SUFFIX)
	cc -c -fPIC -I. -L. -larbel -lm examples/link.c
	cc -shared -fPIC -o link.$(SUFFIX) -L. -I. link.o -larbel


install: arbel libarbel.$(SUFFIX)
	cp arbel $(PREFIX)/bin/
	cp libarbel.$(SUFFIX) $(PREFIX)/lib/
	cp arbel.h $(PREFIX)/include/

