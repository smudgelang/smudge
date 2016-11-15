SMUDGE ?= ../../../dist/build/smudge/smudge
SMUDGE_OPTIONS=--c-no-debug --dot-fmt=Svg

CC=gcc
CFLAGS=-Wall -Wextra -Wno-unused-parameter

.PHONY: clean

all: main

main: main.c turnstyle.o
	$(CC) -o $@ $(CFLAGS) $^

%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) $<

%.c: %.smudge
	$(SMUDGE) $(SMUDGE_OPTIONS) $<

%.svg: %.smudge
	$(SMUDGE) $(SMUDGE_OPTIONS) $<

clean:
	rm -f *.o main turnstyle.c turnstyle.h *_ext.h *.svg
