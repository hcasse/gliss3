GLISS_ARCH=/home/barre/bzr_projects/leon/
ARCH_IRG=$(GLISS_ARCH)/leon.irg
GLISS2_PREFIX=/home/barre/bzr_projects/gliss2/
GLISS2_ATTR=$(GLISS2_PREFIX)/gep/gliss-attr
NMP2NML=$(GLISS2_PREFIX)/gep/gliss-nmp2nml.pl

GNU_TARGET=sparc-elf
GDB_TARGET_PATH=/home/barre/leon-tools/sparc-elf-4.4.2/bin
GDB_NAME=$(GDB_TARGET_PATH)/$(GNU_TARGET)-gdb
PROC_NAME=leon


#do not modify below this line
SRCS = main.c io_module.c gdb_interface.c interface_code_dump_regs.c leon_register.c \
	interface_code_compare_regs.c log.c interface_code_read_vars.c interface_code.c
INCL = io_module.h all_inc.h  interface_code.h  internal.h  leon_register.h

OBJS = $(SRCS:.c=.o)
CC = gcc
CFLAGS = -W -Wall -g -I$(GLISS_ARCH)/include -DGNU_TARGET=\"$(GNU_TARGET)\" -DGDB_NAME=\"$(GDB_NAME)\" -DGLISS_API_H=\<$(PROC_NAME)/api.h\> -DGLISS_REG_H=\"$(PROC_NAME)_register.h\" -DPROC'(x)'=$(PROC_NAME)\#\#x
#TODO change the next line when gliss2 library will be generated in the right place
LDFLAGS = -L$(GLISS_ARCH)/src
LINK = libtool --mode=link $(CC) $(LINKFLAGS) -o $@

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

all : validator2-$(PROC_NAME)

validator2-$(PROC_NAME) : interface_code.h $(INCL) $(GLISS_ARCH)/validator.cfg $(OBJS)
	$(LINK) $(OBJS) -lm  -l$(PROC_NAME) $(LDFLAGS) 

clean:
	rm -f $(OBJS)

distclean: clean
	rm -f validator2-$(PROC_NAME) interface_code.h interface_code.c


interface_code.o: interface_code.c
interface_code.c: interface_code.h
interface_code.h: generate_interface_code.py $(GLISS_ARCH)/validator.cfg
	 python generate_interface_code.py -i $(GLISS_ARCH)/validator.cfg -p $(PROC_NAME)

