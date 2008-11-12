# $Id: Makefile,v 1.6 2008/11/12 20:48:35 casse Exp $
include ../Makefile.head

SOURCES = \
	irg.ml \
	iter.ml \
	lexer.mll \
	sem.ml \
	parser.mly \
	main.ml

OCAML_LIB += unix
OCAML_LIB+=str
$(eval $(call ocaml_prog,irg,$(SOURCES)))

include ../Makefile.tail

doc:
	mkdir autodoc
	ocamldoc -html -i . irg.ml sem.ml iter.ml -d autodoc
