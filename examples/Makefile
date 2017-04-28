SRCS=$(wildcard *.smudge)
COUNTEREXAMPLES=$(wildcard counterexamples/*.smudge)
SMUDGE=../dist/build/smudge/smudge
OBJS=$(addsuffix .svg, $(basename $(SRCS)))
OPTIONS ?= --dot-no-se --dot-fmt=Svg

.PHONY: all clean test

all: $(OBJS) test

test: $(COUNTEREXAMPLES) $(SMUDGE)
	$(foreach f, $(COUNTEREXAMPLES), ! $(SMUDGE) $(OPTIONS) $f &&) true

%.svg: %.smudge $(SMUDGE)
	$(SMUDGE) $(OPTIONS) $<

clean:
	rm -f *.svg *.h *.c
