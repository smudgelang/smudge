SRCS=$(wildcard *.smudge)
COUNTEREXAMPLES=$(wildcard counterexamples/*.smudge)
SMUDGE=../dist/build/smudge/smudge
OBJS=$(addsuffix .svg, $(basename $(SRCS)))
OPTIONS ?= --dot-no-se

.PHONY: all clean test

all: $(OBJS) test

test: $(COUNTEREXAMPLES) $(SMUDGE)
	for f in $(COUNTEREXAMPLES); do ! $(SMUDGE) $$f; done

%.svg: %.smudge $(SMUDGE)
	$(SMUDGE) $(OPTIONS) --dot-fmt=Svg $<

clean:
	rm -f *.svg *.h *.c
