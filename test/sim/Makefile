MEMORY=fast_mem
LOADER=old_elf
CFLAGS=-DTARGET_ENDIANNESS=little -DHOST_ENDIANNESS=big -Wall -g3
CLEAN=gliss.h gliss.c memory.c memory.h loader.c loader.h

all: main

main: main.o memory.o gliss.o loader.o

gliss.h:
	ln -s ../../lib/gliss.h

gliss.c:
	ln -s ../../lib/gliss.c

memory.h:
	ln -s ../../lib/$(MEMORY).h memory.h

memory.c:
	ln -s ../../lib/$(MEMORY).c memory.c

loader.h:
	ln -s ../../lib/$(LOADER).h loader.h

loader.c:
	ln -s ../../lib/$(LOADER).c loader.c

memory.o: memory.h
gliss.o: gliss.h
loader.o: loader.h
main.o: memory.h gliss.h loader.h

clean:
	rm *.o $(CLEAN)
