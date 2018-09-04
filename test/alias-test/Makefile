CLEAN=$(BPATH)/include $(BPATH)/src $(BPATH)/disasm $(BPATH)/sim
GFLAGS=	-m loader:old_elf \
	-m inst_size:inst_size \
	-m code:code \
	-a disasm.c
GEP_DIR=$(PWD)/../../gep

OPATH = $(PWD)
BPATH = ../out/alias-test

all: lib


$(BPATH)/src $(BPATH)/include: $(OPATH)/test.nml
	mkdir -p $(BPATH)
	cd $(BPATH); $(GEP_DIR)/gep $(GFLAGS) $(OPATH)/test.nml -S

lib: $(BPATH)/src $(BPATH)/src/config.h

toto-disasm:
	cd $(BPATH)/disasm; make

toto-sim:
	cd $(BPATH)/sim; make

$(BPATH)/src/config.h: $(OPATH)/config.tpl
	test -d $(BPATH)/src || mkdir $(BPATH)/src
	cp $(OPATH)/config.tpl $(BPATH)/src/config.h

$(BPATH)/src/disasm.c:
	$(GEP_DIR)/gliss-disasm $(OPATH)/test.nml -o $@ -c

clean:
	rm -rf $(CLEAN)
