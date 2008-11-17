# $Id: Makefile,v 1.7 2008/11/17 21:53:35 casse Exp $
include ../Makefile.head

# irg library
LIB_SOURCES = \
	IdMaker.ml \
	irg.ml \
	iter.ml \
	lexer.mll \
	sem.ml \
	parser.mly \
	main.ml
$(eval $(call ocaml_lib,irg,$(LIB_SOURCES)))


# gep command
GEP_SOURCES = gep.ml
gep_LIBS = str unix irg
$(eval $(call ocaml_prog,gep,$(GEP_SOURCES)))


include ../Makefile.tail

doc:
	mkdir autodoc
	ocamldoc -html -i . irg.ml sem.ml iter.ml -d autodoc
