.PHONY: all clean

all: tutorial.pdf

tutorial.pdf: tutorial.rst 1/tutorial.png
	rst2pdf $<

1/tutorial.png:
	cd 1 && $(MAKE)

clean:
	rm -f *.pdf
