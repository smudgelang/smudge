HSFILES=$(wildcard *.hs) $(wildcard Backends/*.hs) $(wildcard Grammars/*.hs) $(wildcard Parsers/*.hs) $(wildcard Unparsers/*.hs) $(wildcard dist/build/autogen/*.hs)

OSTYPE=$(shell uname -o)
ifeq ($(OSTYPE), Cygwin)
SMUDGE_EXE=smudge.exe
else
SMUDGE_EXE=smudge
endif
SMUDGE_TARGET=dist/build/smudge/$(SMUDGE_EXE)

.PHONY: tags clean build examples config newticket release

all: build examples TAGS

tags: TAGS
	echo $(SMUDGE_EXE)
	echo ":ctags" | ghci -v0 `find . -iname \*\.hs | grep -v Setup.hs`

config: dist/setup-config

build: $(SMUDGE_TARGET)

examples: build
	cd examples && $(MAKE) all

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

release: build
	rm -rf dist/release # Make sure it's a clean new release build.
	mkdir dist/release
	cp $(SMUDGE_TARGET) dist/release
	cd examples && $(MAKE) clean
	cp -r examples dist/release
	./clean-tutorial.sh
	cp -r docs/tutorial dist/release
    # There should be a way to to just copy the subdirectories, not the files.
	rm dist/release/tutorial/Makefile dist/release/tutorial/tutorial.rst
	cd docs/tutorial && make tutorial.pdf
	cp docs/tutorial/tutorial.pdf dist/release/tutorial
	cp README dist/release
	./tar-up-release.sh $(SMUDGE_TARGET)


clean:
	rm -rf dist TAGS tags
	cd examples && $(MAKE) clean
