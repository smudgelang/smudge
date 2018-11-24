.PHONY: all clean

BUILD_DIR=$(subst \,/,$(shell stack path --local-install-root))
DIST_PATH=$(BUILD_DIR)/def

all: $(DIST_PATH)/smudge-definition.pdf

$(DIST_PATH):
	mkdir -p $(DIST_PATH)

$(DIST_PATH)/smudge-definition.pdf: smudge-definition.latex $(DIST_PATH)/syntax.latex $(DIST_PATH)/lexer.latex
	pdflatex -output-directory $(DIST_PATH) $<

$(DIST_PATH)/%.latex: %.abnf $(DIST_PATH)
	sh abnftosyntax.sh $< > $@

clean:
	rm -rf $(DIST_PATH)
