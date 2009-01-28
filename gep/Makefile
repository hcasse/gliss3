# $Id: Makefile,v 1.8 2009/01/28 13:43:48 casse Exp $
include ../Makefile.head

OCAML_FLAGS=-I ../irg

GEP_SOURCES = \
	config.ml \
	toc.ml \
	templater.mll \
	fetch.ml \
	decode.ml \
	app.ml \
	gep.ml
gep_LIBS = str unix ../irg/irg
$(eval $(call ocaml_prog,gep,$(GEP_SOURCES)))

DISASM_SOURCES = \
	config.ml \
	toc.ml \
	templater.mll \
	app.ml \
	disasm.ml
gliss_disasm_LIBS = str unix ../irg/irg
$(eval $(call ocaml_prog,gliss_disasm,$(DISASM_SOURCES)))


config.ml:
	echo "let install_dir = \"$(PREFIX)\"" > config.ml
	echo "let source_dir = \"$(PWD)/..\"" >> config.ml

include ../Makefile.tail
