# $Id: Makefile,v 1.8 2008/11/18 10:09:58 casse Exp $
include ../Makefile.head

LIB_SOURCES = \
	IdMaker.ml \
	irg.ml \
	iter.ml \
	lexer.mll \
	sem.ml \
	parser.mly \
	main.ml
$(eval $(call ocaml_lib,irg,$(LIB_SOURCES)))

include ../Makefile.tail

doc:
	mkdir autodoc
	ocamldoc -html -i . irg.ml sem.ml iter.ml -d autodoc
