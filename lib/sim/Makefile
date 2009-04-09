
CFLAGS=-I../include -I../src
LIBADD =  $$(shell bash ../src/$(proc)-config --libs)

$(proc)-sim: $(proc)-sim.o
	$$(CC) $$(LDFLAGS) -o $$@ $$^ $$(LIBADD)
