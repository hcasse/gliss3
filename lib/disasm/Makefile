
CFLAGS=-I../include -I../src
LIBADD = -L../src -l$(proc)

$(proc)-disasm: $(proc)-disasm.o
	$$(CC) $$(LDFLAGS) -o $$@ $$^ $$(LIBADD)
