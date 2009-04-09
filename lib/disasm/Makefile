
CFLAGS=-I../include -I../src
LIBADD += $$(shell bash ../src/$(proc)-config --libs)

$(proc)-disasm: $(proc)-disasm.o
	$$(CC) $$(LDFLAGS) -o $$@ $$^ $$(LIBADD)
