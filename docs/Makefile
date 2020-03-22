doc: arbel.html index.mro.html examples.mro.html
	cat mysite.mro index.mro.html | pushmac > index.html
	cat mysite.mro examples.mro.html | pushmac > examples.html
	makeinfo --html arbel.texi -o arbel-multipage
	makeinfo --html arbel.texi --no-split -o arbel.html

