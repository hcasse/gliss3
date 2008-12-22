MEMORY=fast_mem
CFLAGS=-DTARGET_ENDIANNESS=little -DHOST_ENDIANNESS=big

all: main

main: main.o memory.o

memory.h:
	ln -s ../../lib/$(MEMORY).h memory.h

memory.c:
	ln -s ../../lib/$(MEMORY).c memory.c

memory.o: memory.h
