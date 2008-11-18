# $Id: Makefile,v 1.2 2008/11/18 15:39:32 casse Exp $
include ../Makefile.head

GEP_SOURCES = toc.ml gep.ml
OCAML_FLAGS=-I ../irg
gep_LIBS = str unix ../irg/irg
$(eval $(call ocaml_prog,gep,$(GEP_SOURCES)))


include ../Makefile.tail
