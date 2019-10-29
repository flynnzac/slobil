ifndef PREFIX
	PREFIX=/usr/local/
endif

ifeq ($(OS),Windows_NT)
	SUFFIX=dll
	DOC=xsltproc  --xinclude --output docs/arbel.html --stringparam html.stylesheet arbel.css ~/docbook/docbook-xsl-1.79.1/html/docbook.xsl docs/arbel.dbk
else
	SUFFIX=so
	DOC=xsltproc --xinclude  --output docs/arbel.html --stringparam html.stylesheet arbel.css /usr/share/xml/docbook/stylesheet/docbook-xsl-ns/html/docbook.xsl docs/arbel.dbk
endif

ifdef DEBUG
	COMPILE=cc -B src -o ../arbel arbel.c utility.c save.c parse.c statement.c compute.c copy.c memory.c print.c register.c registry.c state.c type.c operator.c -lreadline -g -lm -Wall -ldl
else
	COMPILE=cc -o ../arbel arbel.c utility.c save.c parse.c statement.c compute.c copy.c memory.c print.c register.c registry.c state.c type.c operator.c -lreadline -O3 -lm -Wall -ldl
endif

arbel: arbel.c operator.c utility.c parse.c statement.c save.c compute.c copy.c memory.c print.c register.c registry.c state.c type.c
	cc -B src -c -fPIC register.c registry.c state.c memory.c copy.c compute.c utility.c save.c statement.c parse.c type.c print.c -O3 -lm -Wall -ldl -lreadline
	cc -B src -o ../libarbel.$(SUFFIX) -fPIC -shared register.o registry.o state.o memory.o copy.o compute.o utility.o save.o statement.o parse.o type.o print.o
	$(COMPILE)

doc: docs/arbel.dbk docs/arbel.css
	$(DOC)
	cat docs/mysite.mro docs/index.mro.html | mro > docs/index.html

examples: examples/link.c libarbel.$(SUFFIX)
	cc -c -fPIC -I. -L. -larbel -lm examples/link.c
	cc -shared -fPIC -o link.$(SUFFIX) -L. -I. link.o -larbel


install: arbel libarbel.$(SUFFIX)
	cp arbel $(PREFIX)/bin/
	cp libarbel.$(SUFFIX) $(PREFIX)/lib/
	cp arbel.h $(PREFIX)/include/

