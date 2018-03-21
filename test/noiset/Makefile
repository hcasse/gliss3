
all: src/noiset.c

src/noiset.c: noiset.nml
	../../gep/gep -no-default -t ./noiset.tpl:src/noiset.c noiset.nml

clean:
	rm -rf src include
