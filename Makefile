HSFILES=$(wildcard *.hs) $(wildcard app/*.hs) $(wildcard src/*/*.hs) $(wildcard src/*/*/*.hs) $(wildcard src/*/*/*/*.hs)

ifeq ($(OS),Windows_NT)
SMUDGE_EXE=smudge.exe
PLATFORM=windows
/=\\
PKGEXT=zip
else
SMUDGE_EXE=smudge
PLATFORM=linux
/=/
PKGEXT=tgz
endif
define cabal_query
$(shell grep "^$(1):" smudge.cabal | cut -d ':' -f 1 --complement | sed -e 's/^\s*//' -e 's/\s*$$//')
endef
SMUDGE_BUILD_DIR=$(subst \,/,$(shell stack path --local-install-root))
SMUDGE_RELEASE_SUBDIR=smudge
SMUDGE_RELEASE_STAGE_DIR=$(SMUDGE_BUILD_DIR)/$(SMUDGE_RELEASE_SUBDIR)
SMUDGE_TARGET=$(SMUDGE_BUILD_DIR)/bin/$(SMUDGE_EXE)
SMUDGE_VERSION=$(call cabal_query,version)

.PHONY: all tags build examples doc \
        release stage package zip tgz \
        newticket todo clean distclean

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

docs/tutorial/tutorial.pdf:
	$(MAKE) -C docs$/tutorial tutorial.pdf

TAGS: $(HSFILES)
	@if command -v hasktags >/dev/null 2>&1; then \
		hasktags `find . -iname \*\.hs | grep -v Setup.hs`; \
	fi

release:
	@read -r -p "Is this release correctly versioned and tagged? [Yn] " REPLY; \
	if [ "$$REPLY" = "n" ]; then echo "Well, do that, then!"; exit 1; fi
	$(MAKE) package

stage: build docs/tutorial/tutorial.pdf
	# Make sure it's a clean new release build.
	rm -rf $(SMUDGE_RELEASE_STAGE_DIR)
	mkdir -p $(SMUDGE_RELEASE_STAGE_DIR)
	cp $(SMUDGE_TARGET) $(SMUDGE_RELEASE_STAGE_DIR)
	$(MAKE) -C examples clean
	cp -r examples $(SMUDGE_RELEASE_STAGE_DIR)
	$(MAKE) -C docs$/tutorial docclean
	cp -r docs/tutorial $(SMUDGE_RELEASE_STAGE_DIR)
	cp CHANGES $(SMUDGE_RELEASE_STAGE_DIR)
	cp LICENSE $(SMUDGE_RELEASE_STAGE_DIR)
	cp README.md $(SMUDGE_RELEASE_STAGE_DIR)

package: $(foreach EXT,$(PKGEXT),smudge-$(SMUDGE_VERSION)-$(PLATFORM).$(EXT))

zip: smudge-$(SMUDGE_VERSION)-$(PLATFORM).zip
smudge-$(SMUDGE_VERSION)-$(PLATFORM).zip: stage
	cd $(SMUDGE_BUILD_DIR) && \
	if type zip >/dev/null 2>&1; then \
	    zip -r $@ $(SMUDGE_RELEASE_SUBDIR); \
	elif type 7z >/dev/null 2>&1; then \
	    7z a $@ $(SMUDGE_RELEASE_SUBDIR); \
	fi
	mv $(SMUDGE_BUILD_DIR)/$@ .

tgz: smudge-$(SMUDGE_VERSION)-$(PLATFORM).tgz
smudge-$(SMUDGE_VERSION)-linux.tgz: stage
	cd $(SMUDGE_BUILD_DIR) && \
	tar -czf $@ $(SMUDGE_RELEASE_SUBDIR)
	mv $(SMUDGE_BUILD_DIR)/$@ .

newticket:
	cd tickets && ./mkticket.sh "$(title)"

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
