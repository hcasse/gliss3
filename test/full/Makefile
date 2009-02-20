CLEAN=include src
GFLAGS=-m \
	sysparm:sysparm-reg32 \
	-m loader:old_elf \
	-m syscall:syscall-linux \
	-m fetch:fetch32 \
	-m decode:decode32 \
	-m inst_size:inst_size \
	-a disasm.c

all: ppc.nml src/config.h gep src/disasm.c lib simu

gep:
	../../gep/gep $(GFLAGS) ppc.nml

lib:
	(cd src; make)

simu: simu.c
	$(CC) $(CFLAGS) $< -Iinclude -o $@ -L src -lppc

src/config.h: config.tpl
	test -d src || mkdir src
	cp config.tpl src/config.h

src/disasm.c:
	../../gep/gliss_disasm ppc.nml -o $@

clean:
	rm -rf $(CLEAN)
