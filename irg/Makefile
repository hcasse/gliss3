# $Id: Makefile,v 1.13 2009/07/02 08:46:58 dubot Exp $
include ../Makefile.head

ifndef NDEBUG
OCAML_FLAGS+=-g
OCAML_LDFLAGS+=-g
endif

LIB_SOURCES = \
	IdMaker.ml \
	irg.ml \
	lexer.mll \
	sem.ml \
	parser.mly \
	iter.ml \
	main.ml
$(eval $(call ocaml_byte_code_lib,irg,$(LIB_SOURCES)))

print_irg_LIBS=str irg
$(eval $(call ocaml_prog,print_irg,print_irg.ml))

mkirg_LIBS=str irg
$(eval $(call ocaml_prog,mkirg,mkirg.ml))

include ../Makefile.tail

doc:
	mkdir autodoc
	ocamldoc -html irg.ml sem.ml iter.ml -d autodoc
