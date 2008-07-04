# $Id: Makefile,v 1.2 2008/07/04 09:47:12 jorquera Exp $
include ../Makefile.head

SOURCES = \
	irg.ml \
	sem.ml \
	lexer.mll \
	parser.mly \
	main.ml

OCAML_LIB += unix
$(eval $(call ocaml_prog,irg,$(SOURCES)))

include ../Makefile.tail
