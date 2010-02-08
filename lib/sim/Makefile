
CFLAGS=-I../include -I../src -g
LIBADD =  $$(shell bash ../src/$(proc)-config --libs)

all: $(proc)-sim

$(proc)-sim: $(proc)-sim.o
	$$(CC) $$(LDFLAGS) -o $$@ $$^ $$(LIBADD)

clean:
	rm -f $(proc)-sim.o

distclean: clean
	rm -f $(proc)-sim
