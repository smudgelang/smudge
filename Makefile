HSFILES=$(wildcard *.hs) $(wildcard Backends/*.hs) $(wildcard dist/build/autogen/*.hs)

.PHONY: tags clean config

all: build tags

tags: TAGS
	echo ":ctags" | ghci -v0 `find . -iname \*\.hs | grep -v Setup.hs`

config: dist/setup-config

build: dist/build/smudge/smudge

# Let Cabal handle dependencies.
dist/setup-config: smudge.cabal
	cabal configure

dist/build/smudge/smudge: config $(HSFILES)
	cabal build

TAGS: $(HSFILES)
	hasktags `find . -iname \*\.hs | grep -v Setup.hs`

clean:
	rm -rf dist TAGS tags
