# $Id: Makefile,v 1.9 2009/01/29 09:46:03 casse Exp $
include ../Makefile.head

ifndef NDEBUG
OCAML_FLAGS+=-g
OCAML_LDFLAGS+=-g
endif

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
