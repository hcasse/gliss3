# $Id: Makefile,v 1.13 2009/09/15 07:50:48 casse Exp $
include ../Makefile.head

OCAML_FLAGS=-I ../irg
ifndef NDEBUG
OCAML_FLAGS+=-g
OCAML_LDFLAGS+=-g
endif

# libgep build
LIBGEP_SOURCES=\
	config.ml \
	toc.ml \
	templater.mll \
	app.ml
$(eval $(call ocaml_byte_code_lib,libgep,$(LIBGEP_SOURCES)))

# GEP build
GEP_SOURCES = \
	fetch.ml \
	decode.ml \
	gep.ml
gep_LIBS = str unix  ../irg/irg libgep
$(eval $(call ocaml_prog,gep,$(GEP_SOURCES)))

gep: ../irg/irg.cma

# gliss-disasm build
DISASM_SOURCES = \
	disasm.ml

gliss-disasm_LIBS = str unix ../irg/irg libgep
$(eval $(call ocaml_prog,gliss-disasm,$(DISASM_SOURCES)))
gliss-disasm: ../irg/irg.cma  libgep.cma


# gliss-attr build
ATTR_SOURCES = \
	gliss-attr.ml

gliss-attr_LIBS = str unix ../irg/irg libgep
$(eval $(call ocaml_prog,gliss-attr,$(ATTR_SOURCES)))
gliss-attr: ../irg/irg.cma libgep.cma


# config build
config.ml:
	echo "let install_dir = \"$(PREFIX)\"" > config.ml
	echo "let source_dir = \"$(PWD)/..\"" >> config.ml

include ../Makefile.tail
