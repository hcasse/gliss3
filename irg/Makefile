# $Id: Makefile,v 1.4 2008/11/04 12:06:55 barre Exp $
include ../Makefile.head

SOURCES = \
	irg.ml \
	infos.ml \
	lexer.mll \
	sem.ml \
	parser.mly \
	main.ml

OCAML_LIB += unix
OCAML_LIB+=str
$(eval $(call ocaml_prog,irg,$(SOURCES)))

include ../Makefile.tail
