SRCS=$(wildcard *.smudge)
COUNTEREXAMPLES=$(wildcard counterexamples/*.smudge)
SMUDGEEXE=$(shell stack path --local-install-root)/bin/smudge
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
