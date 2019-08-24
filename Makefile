arbel: arbel.c operator.c primitive.c utility.c parse.c statement.c
	cc -o arbel arbel.c operator.c primitive.c utility.c parse.c statement.c -lreadline -g -lm -Wall

doc: docs/arbel.dbk docs/arbel.css
	xsltproc  --output docs/arbel.html --stringparam html.stylesheet arbel.css /usr/share/xml/docbook/stylesheet/docbook-xsl-ns/html/docbook.xsl docs/arbel.dbk
	cat docs/mysite.mro docs/index.mro.html | mro > docs/index.html



