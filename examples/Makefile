SRCS=$(wildcard *.smudge)
SMUDGE=../dist/build/smudge/smudge
OBJS=$(addsuffix .svg, $(basename $(SRCS)))
OPTIONS ?= --dot-no-se

.PHONY: all clean

all: $(OBJS)

%.svg: %.smudge $(SMUDGE)
	$(SMUDGE) $(OPTIONS) --dot-fmt=Svg $<

clean:
	rm -f *.svg *.h *.c
