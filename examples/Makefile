SRCS=$(wildcard *.smudge)
COUNTEREXAMPLES=counterexamples/badsideeffectname.smudge counterexamples/badstatename.smudge counterexamples/notransientstatecycles.smudge counterexamples/oneinitialstate.smudge counterexamples/uniquestatenames.smudge
UNINSTANTIABLE=counterexamples/uninstantiabletypes.smudge
SMUDGE=../dist/build/smudge/smudge
OBJS=$(addsuffix .svg, $(basename $(SRCS)))
OPTIONS ?= --dot-no-se

.PHONY: all clean test specialflower

all: $(OBJS) test

specialflower: $(UNINSTANTIABLE)
	! $(SMUDGE) --strict $(OPTIONS) --dot-fmt=Svg  $<

test: $(COUNTEREXAMPLES) $(SMUDGE) specialflower
	$(foreach f, $(COUNTEREXAMPLES), ! $(SMUDGE) $f &&) true

%.svg: %.smudge $(SMUDGE)
	$(SMUDGE) $(OPTIONS) --dot-fmt=Svg $<

clean:
	rm -f *.svg *.h *.c
