.PHONY: NMP

#NMP: NMP.html
#	htmldoc  $< --book -t htmlsep -d NMP

all: NMP.html manual.html

%.html: %.t2t
	./txt2tags -t xhtml -i $< -o $@

