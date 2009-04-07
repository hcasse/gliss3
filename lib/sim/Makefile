
CFLAGS=-I../include -I../src
LIBADD = -L../src -l$(proc)

$(proc)-sim: $(proc)-sim.o
	$$(CC) $$(LDFLAGS) -o $$@ $$^ $$(LIBADD)
