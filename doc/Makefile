.PHONY: NMP

all: NMP.html gliss.html

%.html: %.thot
	../../thot/thot.py -t html $<

gliss.html: $(wildcard gliss-*.thot)
