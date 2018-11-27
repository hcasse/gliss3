# $Id: Makefile,v 1.15 2009/07/31 09:09:42 casse Exp $
include ../Makefile.head

OCAMLYACC_FLAGS = -v

ifndef NDEBUG
OCAML_FLAGS+=-g
OCAML_LDFLAGS+=-g
endif

LIB_SOURCES = \
	config.ml \
	IdMaker.ml \
	irg.ml \
	irgio.ml \
	instantiate.ml \
	lexer.mll \
	sem.ml \
	iter.ml \
	parser.mly \
	irgUtil.ml
$(eval $(call ocaml_byte_code_lib,irg,$(LIB_SOURCES)))

print_irg_LIBS=unix str irg
$(eval $(call ocaml_prog,print_irg,print_irg.ml))

mkirg_LIBS=unix str irg
$(eval $(call ocaml_prog,mkirg,mkirg.ml))

CLEAN += config.ml

include ../Makefile.tail

OCAML_MAJOR=$(shell ocaml -version | cut -f5 -d" " | cut -f1 -d.)
OCAML_MINOR=$(shell ocaml -version | cut -f5 -d" " | cut -f2 -d.)
OCAML_VERSION=$(shell expr $(OCAML_MAJOR) \* 100 + $(OCAML_MINOR))

info:
	@echo "OCAML_MAJOR=$(OCAML_MAJOR)"
	@echo "OCAML_MAJOR=$(OCAML_MINOR)"
	@echo "OCAML_VERSION=$(OCAML_VERSION)"


# generate config.ml
VERSION=$(shell cat ../VERSION)
config.ml: ../VERSION
	echo "let install_dir = \"$(PREFIX)\"" > config.ml
	echo "let source_dir = \"$(CURDIR)/..\"" >> config.ml
	echo "let version = \"$(VERSION)\"" >> config.ml
	if [ $(OCAML_VERSION) -ge 403 ]; then \
		echo "let lowercase = String.lowercase_ascii" >> config.ml; \
		echo "let uppercase = String.uppercase_ascii" >> config.ml; \
		echo "let char_lowercase = Char.lowercase_ascii" >> config.ml; \
		echo "let char_uppercase = Char.uppercase_ascii" >> config.ml; \
	else \
		echo "let lowercase = String.lowercase" >> config.ml; \
		echo "let uppercase = String.uppercase" >> config.ml; \
		echo "let char_lowercase = Char.lowercase" >> config.ml; \
		echo "let char_uppercase = Char.uppercase" >> config.ml; \
	fi
