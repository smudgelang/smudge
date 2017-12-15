HSFILES=$(wildcard *.hs) $(wildcard app/*.hs) $(wildcard src/*/*.hs) $(wildcard src/*/*/*.hs) $(wildcard src/*/*/*/*.hs)

ifeq ($(OS),Windows_NT)
SMUDGE_EXE=smudge.exe
PLATFORM=windows
/=\\
else
SMUDGE_EXE=smudge
PLATFORM=linux
/=/
endif
SMUDGE_BUILD_DIR=$(subst \,/,$(shell stack path --local-install-root))
SMUDGE_RELEASE_DIR=release
SMUDGE_RELEASE_SUBDIR=smudge
SMUDGE_RELEASE_STAGE_DIR=$(SMUDGE_RELEASE_DIR)/$(SMUDGE_RELEASE_SUBDIR)
SMUDGE_TARGET=$(SMUDGE_BUILD_DIR)/bin/$(SMUDGE_EXE)

.PHONY: all tags build examples doc newticket release todo clean distclean

all: build examples doc TAGS

tags: TAGS
	echo ":ctags" | ghci -v0 `find . -iname \*\.hs | grep -v Setup.hs`

build: $(SMUDGE_TARGET)

examples: build
	$(MAKE) -C examples all

doc:
	$(MAKE) -C docs$/tutorial tutorial.pdf
	$(MAKE) -C docs$/definition all

$(SMUDGE_TARGET): smudge.cabal stack.yaml $(HSFILES)
	stack $(STACK_FLAGS) build $(CABAL_FLAGS)

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
	mkdir $(SMUDGE_RELEASE_STAGE_DIR)
	cp $(SMUDGE_TARGET) $(SMUDGE_RELEASE_STAGE_DIR)
	$(MAKE) -C docs$/tutorial tutorial.pdf
	$(MAKE) -C docs$/tutorial docclean
	cp -r examples $(SMUDGE_RELEASE_STAGE_DIR)
	cp -r docs$/tutorial $(SMUDGE_RELEASE_STAGE_DIR)
	cp CHANGES $(SMUDGE_RELEASE_STAGE_DIR)
	cp LICENSE $(SMUDGE_RELEASE_STAGE_DIR)
	cp README.md $(SMUDGE_RELEASE_STAGE_DIR)
	./tar-up-release.sh $(SMUDGE_RELEASE_DIR) $(SMUDGE_RELEASE_SUBDIR) $(SMUDGE_TARGET) $(PLATFORM)

todo:
	@find roadmap/$V | while read -r fn; do find -L tickets/ -xdev -samefile $$fn; done

clean:
	stack clean
	rm -rf TAGS tags
	$(MAKE) -C examples clean
	$(MAKE) -C docs$/tutorial clean
	$(MAKE) -C docs$/definition clean

distclean: clean
	rm -rf .stack-work
