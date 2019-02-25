
HEADERS=csim.h
SOURCES=csim.c
OBJECTS=$(SOURCES:.c=.o)

CFLAGS=-g3 -Wall

all: test-csim

clean:
	-rm -rf $(OBJECTS)

test-csim: $(OBJECTS)
	$(CC) -o $@ $(OBJECTS)

csim.o: csim.h
