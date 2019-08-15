arbel: arbel.c operator.c primitive.c utility.c parse.c save.c
	cc -o arbel arbel.c operator.c primitive.c utility.c parse.c save.c -lreadline -g -lm

doc: docs/arbel.dbk docs/arbel.css
	xsltproc  --output docs/arbel.html --stringparam html.stylesheet arbel.css /usr/share/xml/docbook/stylesheet/docbook-xsl-ns/html/docbook.xsl docs/arbel.dbk
