HSFILES=$(wildcard *.hs) $(wildcard Backends/*.hs) $(wildcard Grammars/*.hs) $(wildcard Parsers/*.hs) $(wildcard Unparsers/*.hs) $(wildcard dist/build/autogen/*.hs)

.PHONY: tags clean build examples config

all: build examples tags

tags: TAGS
	echo ":ctags" | ghci -v0 `find . -iname \*\.hs | grep -v Setup.hs`

config: dist/setup-config

build: dist/build/smudge/smudge

examples: build
	cd examples && $(MAKE) all

# Let Cabal handle dependencies.
dist/setup-config: smudge.cabal
	cabal configure

dist/build/smudge/smudge: config $(HSFILES)
	cabal build

TAGS: $(HSFILES)
	@if command -v hasktags >/dev/null 2>&1; then \
		hasktags `find . -iname \*\.hs | grep -v Setup.hs`; \
	fi

clean:
	rm -rf dist TAGS tags
	cd examples && $(MAKE) clean
