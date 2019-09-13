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

.FORCE:

arbel: .FORCE
	make -C src

doc: docs/arbel.dbk docs/arbel.css
	$(DOC)
	cat docs/mysite.mro docs/index.mro.html | mro > docs/index.html

examples: examples/link.c libarbel.$(SUFFIX)
	cc -c -fPIC -I. -L. -larbel -lm examples/link.c
	cc -shared -fPIC -o link.$(SUFFIX) -L. -I. link.o -larbel

install: arbel libarbel.$(SUFFIX) src/arbel.h
	cp arbel $(PREFIX)/bin/
	cp libarbel.$(SUFFIX) $(PREFIX)/lib/
	cp src/arbel.h $(PREFIX)/include/

