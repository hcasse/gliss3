# $Id: Makefile,v 1.3 2008/07/22 09:49:09 jorquera Exp $
include ../Makefile.head

SOURCES = \
	irg.ml \
	lexer.mll \
	sem.ml \
	parser.mly \
	main.ml

OCAML_LIB += unix
OCAML_LIB+=str
$(eval $(call ocaml_prog,irg,$(SOURCES)))

include ../Makefile.tail
