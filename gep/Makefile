# $Id: Makefile,v 1.7 2009/01/27 21:56:53 casse Exp $
include ../Makefile.head

GEP_SOURCES = \
	config.ml \
	toc.ml \
	templater.mll \
	fetch.ml \
	decode.ml \
	app.ml gep.ml
OCAML_FLAGS=-I ../irg
gep_LIBS = str unix ../irg/irg
$(eval $(call ocaml_prog,gep,$(GEP_SOURCES)))

config.ml:
	echo "let install_dir = \"$(PREFIX)\"" > config.ml
	echo "let source_dir = \"$(PWD)/..\"" >> config.ml

include ../Makefile.tail
