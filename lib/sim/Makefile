
CFLAGS=-I../include -I../src -g
LIBADD =  $$(shell bash ../src/$(proc)-config --libs)

$(proc)-sim: $(proc)-sim.o
	$$(CC) $$(CFLAGS) -o $$@ $$^ $$(LIBADD)

clean:
	rm -f $(proc)-sim.o

distclean: clean
	rm -f $(proc)-sim
