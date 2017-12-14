ifeq ($(OS),Windows_NT)
S=\\
MKDIR=-mkdir
MV=cmd /c move
DIR=dir
else
S=/
MKDIR=mkdir -p
MV=mv
DIR=ls -lR
endif

.PHONY: all clean

BUILD_DIR=$(shell stack path --local-install-root)
DIST_PATH=$(BUILD_DIR)$Sdef

all: $(DIST_PATH)$Ssmudge-definition.pdf

$(DIST_PATH)$Ssmudge-definition.pdf: smudge-definition.latex $(DIST_PATH)$Ssyntax.latex
	$(MKDIR) $(DIST_PATH)
	pdflatex -output-directory $(DIST_PATH) $<

$(DIST_PATH)$Ssyntax.latex: syntax.abnf
	$(MKDIR) $(DIST_PATH)
	sh abnftosyntax.sh $< > tmp
	$(MV) tmp $@

clean:
	rm -rf $(DIST_PATH)
