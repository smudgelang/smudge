SMUDGE ?= ../../dist/build/smudge/smudge
SMUDGE_OPTIONS=--dot-fmt=Svg
SMUDGE_OPTIONS_PNG=--dot-fmt=Png

SMUDGE_FILES=$(basename $(wildcard *.smudge))
ALWAYS_GENERATED=.h .c _ext.h
GENERATED_EXTENSIONS=$(ALWAYS_GENERATED) _ext.c .gv .png .svg
GENERATED_FILES=$(foreach x,$(GENERATED_EXTENSIONS), $(addsuffix $(x), $(SMUDGE_FILES)))

EXAMPLE_MAINS=fixed broken events use_side_effects payloads dynamic_payloads use_enter_exit use_transitionless use_transient message_passing multism any_state any_event
EXAMPLE_DEPENDS=$(addsuffix .d,$(EXAMPLE_MAINS))

CC=gcc
CFLAGS=-Wall -Wextra -Wno-unused-parameter

.PHONY: clean test

all: $(EXAMPLE_MAINS)

%.d: %.c # This is lifted from the GNU Make tutorial, with slight modification.
	@set -e; rm -f $@; \
	  $(CC) -MM -MG $(CFLAGS) $< > $@.$$$$; \
	  sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	  rm -f $@.$$$$

%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) $<

$(addprefix %,$(ALWAYS_GENERATED)): %.smudge
	$(SMUDGE) $(SMUDGE_OPTIONS) $^

%_ext.c: %.smudge
	$(SMUDGE) $(SMUDGE_OPTIONS_PNG) --c-stubs $^

%.svg: %.smudge
	$(SMUDGE) $(SMUDGE_OPTIONS) $^

%.png: %.smudge
	$(SMUDGE) $(SMUDGE_OPTIONS_PNG) $^

events: events.o
fixed: fixed.o
events fixed: initial.o
	$(CC) -o $@ $(CFLAGS) $^

broken: initial_ext.o initial.o broken.o
	$(CC) -o $@ $(CFLAGS) $^

use_side_effects: side_effects.o use_side_effects.o
	$(CC) -o $@ $(CFLAGS) $^

payloads: side_effects.o payloads.o
	$(CC) -o $@ $(CFLAGS) $^

dynamic_payloads: side_effects.o dynamic_payloads.o queue.o
	$(CC) -o $@ $(CFLAGS) $^

use_enter_exit: enter_exit.o use_enter_exit.o
	$(CC) -o $@ $(CFLAGS) $^

use_transitionless: transitionless.o use_transitionless.o
	$(CC) -o $@ $(CFLAGS) $^

use_transient: transient.o use_transient.o
	$(CC) -o $@ $(CFLAGS) $^

message_passing: side_effects.o message_passing.o queue.o
	$(CC) -o $@ $(CFLAGS) $^

multism: msm.o multism.o queue.o
	$(CC) -o $@ $(CFLAGS) $^

any_state: default_states.o any_state.o queue.o
	$(CC) -o $@ $(CFLAGS) $^

any_event: default_events.o any_event.o queue.o
	$(CC) -o $@ $(CFLAGS) $^

tutorial.pdf: tutorial.rst initial.png side_effects.png enter_exit.png transitionless.png transient.png msm.png default_states.png
	rst2pdf tutorial.rst

docclean:
	rm -f *.o main $(GENERATED_FILES) $(EXAMPLE_MAINS) $(EXAMPLE_DEPENDS)

clean: docclean
	rm -f tutorial.pdf

