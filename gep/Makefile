# $Id: Makefile,v 1.11 2009/04/07 13:09:59 casse Exp $
include ../Makefile.head

OCAML_FLAGS=-I ../irg
ifndef NDEBUG
OCAML_FLAGS+=-g
OCAML_LDFLAGS+=-g
endif

# GEP build
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

# gliss-disasm build
DISASM_SOURCES = \
	config.ml \
	toc.ml \
	templater.mll \
	app.ml \
	disasm.ml

gliss-disasm_LIBS = str unix ../irg/irg
$(eval $(call ocaml_prog,gliss-disasm,$(DISASM_SOURCES)))
gliss-disasm: ../irg/irg.cma


# config build
config.ml:
	echo "let install_dir = \"$(PREFIX)\"" > config.ml
	echo "let source_dir = \"$(PWD)/..\"" >> config.ml

include ../Makefile.tail
