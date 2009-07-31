# $Id: Makefile,v 1.3 2009/07/31 09:09:42 casse Exp $
include Makefile.head

SUBDIRS=irg gep

include Makefile.tail

DOCS = \
	irg/irg.ml \
	irg/sem.ml \
	irg/iter.ml \
	gep/toc.ml \
	gep/app.ml
DOCFLAGS = \
	-I irg -I gep

mkdoc:
	test -d || mkdir autodoc
	ocamldoc -html -d autodoc $(DOCFLAGS) $(DOCS)

