CLEAN=include src disasm ppc.nml
GFLAGS=-m \
	sysparm:sysparm-reg32 \
	-m loader:old_elf \
	-m syscall:syscall-linux \
	-m fetch:fetch32 \
	-m decode:decode32 \
	-m inst_size:inst_size \
	-m code:code \
	-a disasm.c

all: ppc.nml src/config.h gep src/disasm.c lib ppc-disasm simu

ppc.nml: ppc.nmp oea_instr.nmp uisa_fp_instr.nmp  vea_instr.nmp
	../../gep/gliss-nmp2nml.pl $< $@

gep:
	../../gep/gep $(GFLAGS) ppc.nml

lib:
	(cd src; make)

simu: simu.c
	$(CC) $(CFLAGS) $< -Iinclude -o $@ -L src -lppc

ppc-disasm:
	cd disasm; make

src/config.h: config.tpl
	test -d src || mkdir src
	cp config.tpl src/config.h

src/disasm.c:
	../../gep/gliss_disasm ppc.nml -o $@ -c

clean:
	rm -rf $(CLEAN)
