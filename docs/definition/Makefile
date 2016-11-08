.PHONY: all clean

DIST_PATH=../../dist/def

all: $(DIST_PATH)/smudge-definition.pdf

$(DIST_PATH)/smudge-definition.pdf: smudge-definition.latex
	mkdir -p $(DIST_PATH)
	pdflatex -output-directory $(DIST_PATH) smudge-definition.latex

clean:
	rm -rf $(DIST_PATH)
