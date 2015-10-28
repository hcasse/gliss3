#
# GLISS2 Prototype Makefile
# Copyright (C) 2015  IRIT - UPS
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

-include config.mk 

############ ISA CONFIGURATION HERE #############

ARCH=

# main NMP file here
MAIN_NMP	=	

# list of NMP files
NMP = nmp/$(MAIN_NMP) \

############ ISA CONFIGURATION END #############


# definitions
ifdef WITH_DYNLIB
REC_FLAGS = WITH_DYNLIB=1
endif

GFLAGS= \
	-m loader:old_elf \
	-m code:code \
	-m env:void_env \
	-m sys_call:extern/sys_call \
	-v \
	-a disasm.c \
	-S \
	-switch \
	-D

ifdef WITH_IO
GFLAGS += -m mem:io_mem
else
GFLAGS += -m mem:vfast_mem
endif

# goals definition
GOALS		=
SUBDIRS		=	src
CLEAN		=	$(ARCH).nml $(ARCH).irg nmp/state.nmp
DISTCLEAN	=	include src $(CLEAN) config.mk
LIB_DEPS	=	include/$(ARCH)/config.h

ifdef WITH_DISASM
GOALS		+=	$(ARCH)-disasm
SUBDIRS		+=	disasm
DISTCLEAN	+= 	disasm
GFLAGS		+= -a disasm.c
LIB_DEPS	+= src/disasm.c 
endif

ifdef WITH_SIM
GOALS		+=	$(ARCH)-sim
SUBDIRS		+=	sim
DISTCLEAN	+=	sim
endif


# rules
all: lib $(GOALS)

$(ARCH).irg: $(NMP)
	cd nmp &&  ../$(GLISS_PREFIX)/irg/mkirg $(MAIN_NMP) ../$@  && cd ..

src include: $(ARCH).irg
	$(GLISS_PREFIX)/gep/gep $(GFLAGS) $<

lib: src $(LIB_DEPS)
	(cd src; make $(REC_FLAGS))

$(ARCH)-disasm:
	cd disasm; make

include/$(ARCH)/config.h: config.tpl
	test -d src || mkdir src
	cp config.tpl $@

src/disasm.c: $(ARCH).irg
	$(GLISS_PREFIX)/gep/gliss-disasm $(ARCH).irg -o $@ -c

$(ARCH)-sim:
	cd sim; make

clean:
	rm -rf $(CLEAN)

distclean:
	rm -Rf $(DISTCLEAN) $(ARCH).irg $(ARCH).out

config: config.mk

config.mk:
	cp config.mk.in config.mk
	echo "Created config.mk. Edit it for configuration."

	