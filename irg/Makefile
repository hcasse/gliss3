# $Id: Makefile,v 1.10 2009/01/29 10:25:26 casse Exp $
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
$(eval $(call ocaml_byte_code_lib,irg,$(LIB_SOURCES)))

print_irg_LIBS=str irg
$(eval $(call ocaml_prog,print_irg,print_irg.ml))

include ../Makefile.tail

doc:
	mkdir autodoc
	ocamldoc -html -i . irg.ml sem.ml iter.ml -d autodoc
