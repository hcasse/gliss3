# $Id: Makefile,v 1.15 2009/07/31 09:09:42 casse Exp $
include ../Makefile.head

OCAMLYACC_FLAGS = -v

ifndef NDEBUG
OCAML_FLAGS+=-g
OCAML_LDFLAGS+=-g
endif

LIB_SOURCES = \
	templater.mll
$(eval $(call ocaml_byte_code_lib,gen,$(LIB_SOURCES)))

print_irg_LIBS=unix str irg
include ../Makefile.tail
