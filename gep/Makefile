# $Id: Makefile,v 1.1 2008/11/18 10:09:58 casse Exp $
include ../Makefile.head

GEP_SOURCES = gep.ml
OCAML_FLAGS=-I ../irg
gep_LIBS = str unix ../irg/irg
$(eval $(call ocaml_prog,gep,$(GEP_SOURCES)))


include ../Makefile.tail
