.PHONY: all clean

DIST_PATH=$(shell stack path --local-install-root)/def

all: $(DIST_PATH)/smudge-definition.pdf

$(DIST_PATH)/smudge-definition.pdf: smudge-definition.latex $(DIST_PATH)/syntax.latex
	mkdir -p $(DIST_PATH)
	pdflatex -output-directory $(DIST_PATH) $<

$(DIST_PATH)/syntax.latex: syntax.abnf
	mkdir -p $(DIST_PATH)
	./abnftosyntax.sh $< > $@

clean:
	rm -rf $(DIST_PATH)
