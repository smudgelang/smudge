HSFILES=$(wildcard *.hs) $(wildcard app/*.hs) $(wildcard src/*/*.hs) $(wildcard src/*/*/*.hs) $(wildcard src/*/*/*/*.hs)

OSTYPE=$(shell uname -o)
ifeq ($(OSTYPE), Cygwin)
SMUDGE_EXE=smudge.exe
PLATFORM=windows
else
SMUDGE_EXE=smudge
PLATFORM=linux
endif
SMUDGE_BUILD_DIR=$(shell stack path --local-install-root)
SMUDGE_RELEASE_DIR=$(SMUDGE_BUILD_DIR)/release
SMUDGE_TARGET=$(SMUDGE_BUILD_DIR)/bin/$(SMUDGE_EXE)

.PHONY: all tags build examples doc newticket release todo clean distclean

all: build examples doc TAGS

tags: TAGS
	echo ":ctags" | ghci -v0 `find . -iname \*\.hs | grep -v Setup.hs`

build: $(SMUDGE_TARGET)

examples: build
	$(MAKE) -C examples all

doc:
	$(MAKE) -C docs/tutorial tutorial.pdf
	$(MAKE) -C docs/definition all

$(SMUDGE_TARGET): smudge.cabal stack.yaml $(HSFILES)
	stack build

TAGS: $(HSFILES)
	@if command -v hasktags >/dev/null 2>&1; then \
		hasktags `find . -iname \*\.hs | grep -v Setup.hs`; \
	fi

newticket:
	cd tickets && ./mkticket.sh "$(title)"

release: build doc
	@read -r -p "Is this release correctly versioned and tagged? [Yn] " REPLY; \
	if [ "$$REPLY" = "n" ]; then echo "Well, do that, then!"; exit 1; fi
	rm -rf $(SMUDGE_RELEASE_DIR) # Make sure it's a clean new release build.
	mkdir $(SMUDGE_RELEASE_DIR)
	cp $(SMUDGE_TARGET) $(SMUDGE_RELEASE_DIR)
	$(MAKE) -C docs/tutorial tutorial.pdf
	$(MAKE) -C docs/tutorial docclean
	cp -r examples $(SMUDGE_RELEASE_DIR)
	cp -r docs/tutorial $(SMUDGE_RELEASE_DIR)
	cp LICENSE $(SMUDGE_RELEASE_DIR)
	cp README $(SMUDGE_RELEASE_DIR)
	./tar-up-release.sh $(SMUDGE_BUILD_DIR) $(SMUDGE_RELEASE_DIR) $(SMUDGE_TARGET) $(PLATFORM)

todo:
	@find roadmap/$V | while read -r fn; do find -L tickets/ -xdev -samefile $$fn; done

clean:
	stack clean
	rm -rf TAGS tags
	$(MAKE) -C examples clean
	$(MAKE) -C docs/tutorial clean
	$(MAKE) -C docs/definition clean

distclean: clean
	rm -rf .stack-work
