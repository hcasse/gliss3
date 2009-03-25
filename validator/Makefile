
GNU_TARGET=powerpc-linux-gnu
GLISS_ARCH=ppc


#do not modify below this line
SRCS = main.c gdb_interface.c log.c interface_code_create_vars.c interface_code_read_vars.c interface_code_compare_regs.c interface_code_dump_regs.c interface_code_init_gdb_regs.c
OBJS = $(SRCS:.c=.o)
CC = gcc
CFLAGS = -W -Wall -O3 -g -I../$(GLISS_ARCH)/include -DGNU_TARGET=\"$(GNU_TARGET)\"
LDFLAGS = -L../$(GLISS_ARCH)/lib
LINK = libtool --mode=link $(CC) $(LINKFLAGS) -o $@

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)


../$(GLISS_ARCH)/validator-$(GLISS_ARCH) : interface_code_create_vars.c interface_code_read_vars.c interface_code.h interface_code_compare_regs.c interface_code_dump_regs.c interface_code_init_gdb_regs.c $(OBJS) ../$(GLISS_ARCH)/validator.cfg
	$(LINK) $(OBJS) -lemul -lm ../gel/src/libgel_ppc.la $(LDFLAGS)

	
love :
	@echo not war.

.PHONY: clean interface_code_read_vars.c interface_code_create_vars.c interface_code_compare_regs.c interface_code_dump_regs.c interface_code.h

clean:
	rm *.o


interface_code_read_vars.c : generate_interface_code.py ../$(GLISS_ARCH)/validator.cfg
	python generate_interface_code.py -i ../$(GLISS_ARCH)/validator.cfg