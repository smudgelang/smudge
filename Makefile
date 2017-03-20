HSFILES=$(wildcard *.hs) $(wildcard Backends/*.hs) $(wildcard Grammars/*.hs) $(wildcard Parsers/*.hs) $(wildcard Unparsers/*.hs) $(wildcard dist/build/autogen/*.hs)

OSTYPE=$(shell uname -o)
ifeq ($(OSTYPE), Cygwin)
SMUDGE_EXE=smudge.exe
PLATFORM=windows
else
SMUDGE_EXE=smudge
PLATFORM=linux
endif
SMUDGE_TARGET=dist/build/smudge/$(SMUDGE_EXE)

.PHONY: all tags build examples doc config newticket release todo clean

all: build examples doc TAGS

tags: TAGS
	echo ":ctags" | ghci -v0 `find . -iname \*\.hs | grep -v Setup.hs`

config: dist/setup-config

build: $(SMUDGE_TARGET)

examples: build
	$(MAKE) -C examples all

doc:
	$(MAKE) -C docs/tutorial tutorial.pdf
	$(MAKE) -C docs/definition all

# Let Cabal handle dependencies.
dist/setup-config: smudge.cabal
	cabal configure

$(SMUDGE_TARGET): config $(HSFILES)
	cabal build

TAGS: $(HSFILES)
	@if command -v hasktags >/dev/null 2>&1; then \
		hasktags `find . -iname \*\.hs | grep -v Setup.hs`; \
	fi

newticket:
	cd tickets && ./mkticket.sh "$(title)"

release: build doc
	@read -r -p "Is this release correctly versioned and tagged? [Yn] " REPLY; \
	if [ "$$REPLY" = "n" ]; then echo "Well, do that, then!"; exit 1; fi
	rm -rf dist/release # Make sure it's a clean new release build.
	mkdir dist/release
	cp $(SMUDGE_TARGET) dist/release
	$(MAKE) -C docs/tutorial tutorial.pdf
	$(MAKE) -C docs/tutorial docclean
	cp -r examples dist/release
	cp -r docs/tutorial dist/release
	cp README dist/release
	./tar-up-release.sh $(SMUDGE_TARGET) $(PLATFORM)

todo:
	@find roadmap/$V | while read -r fn; do find -L tickets/ -xdev -samefile $$fn; done

clean:
	rm -rf dist TAGS tags
	$(MAKE) -C examples clean
	$(MAKE) -C docs/tutorial clean
	$(MAKE) -C docs/definition clean
