# $Id: Makefile,v 1.10 2009/03/07 13:02:15 casse Exp $
include ../Makefile.head

OCAML_FLAGS=-I ../irg
ifndef NDEBUG
OCAML_FLAGS+=-g
OCAML_LDFLAGS+=-g
endif

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

gep: ../irg/irg.cma

DISASM_SOURCES = \
	config.ml \
	toc.ml \
	templater.mll \
	app.ml \
	disasm.ml
gliss_disasm_LIBS = str unix ../irg/irg
$(eval $(call ocaml_prog,gliss_disasm,$(DISASM_SOURCES)))

gliss_disasm: ../irg/irg.cma

config.ml:
	echo "let install_dir = \"$(PREFIX)\"" > config.ml
	echo "let source_dir = \"$(PWD)/..\"" >> config.ml

include ../Makefile.tail
