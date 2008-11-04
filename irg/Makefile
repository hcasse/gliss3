# $Id: Makefile,v 1.5 2008/11/04 14:04:47 barre Exp $
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
