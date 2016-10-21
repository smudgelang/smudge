.PHONY: all clean

DIST_PATH=../../dist/doc

all: $(DIST_PATH)/tutorial.pdf

$(DIST_PATH)/tutorial.pdf: tutorial.rst $(DIST_PATH)/1/tutorial.png
	rst2pdf $<

$(DIST_PATH)1/tutorial.png: $(DIST_PATH)/1/Makefile
	cd $(DIST_PATH)
	cd 1 && $(MAKE)

$(DIST_PATH)/1/Makefile:
	mkdir $(DIST_PATH)
	find . -maxdepth 1 -type d | xargs cp -r $(DIST_PATH)

clean:
	rm -rf $(DIST_PATH)
	rm -f *.pdf
