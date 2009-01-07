CLEAN=include src
GFLAGS=-m sysparm:sysparm-reg32 -m loader:old_elf -m syscall:syscall-linux

all: ppc.nml src/config.h 
	../../gep/gep $(GFLAGS) ppc.nml

src/config.h: config.tpl
	test -d src || mkdir src
	cp config.tpl src/config.h

clean:
	rm -rf $(CLEAN)
