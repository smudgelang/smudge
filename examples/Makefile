SRCS=$(wildcard *.smudge)
COUNTEREXAMPLES=$(wildcard counterexamples/*.smudge)
ifeq ($(OS),Windows_NT)
SMUDGE_EXE=smudge.exe
else
SMUDGE_EXE=smudge
endif
SMUDGE_BUILD_DIR=$(subst \,/,$(shell stack path --local-install-root))
SMUDGEEXE=$(SMUDGE_BUILD_DIR)/bin/$(SMUDGE_EXE)
SMUDGE=stack exec smudge --
OBJS=$(addsuffix .svg, $(basename $(SRCS)))
OPTIONS ?= --dot-no-se --dot-fmt=Svg

.PHONY: all clean test

all: $(OBJS) test

test: $(COUNTEREXAMPLES) $(SMUDGEEXE)
	$(foreach f, $(COUNTEREXAMPLES), ! $(SMUDGE) $(OPTIONS) $f &&) true

%.svg: %.smudge $(SMUDGEEXE)
	$(SMUDGE) $(OPTIONS) $<

clean:
	rm -f *.svg *.h *.c
