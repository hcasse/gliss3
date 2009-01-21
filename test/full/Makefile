CLEAN=include src
GFLAGS=-m sysparm:sysparm-reg32 -m loader:old_elf -m syscall:syscall-linux

all: ppc.nml src/config.h gep simu

gep:
	../../gep/gep $(GFLAGS) ppc.nml
	(cd src; make)

simu: simu.c
	$(CC) $(CFLAGS) $< -Iinclude -o $@ -L src -lppc

src/config.h: config.tpl
	test -d src || mkdir src
	cp config.tpl src/config.h

clean:
	rm -rf $(CLEAN)
