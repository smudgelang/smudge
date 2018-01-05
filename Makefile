HSFILES=$(wildcard *.hs) $(wildcard app/*.hs) $(wildcard src/*/*.hs) $(wildcard src/*/*/*.hs) $(wildcard src/*/*/*/*.hs)

ifeq ($(OS),Windows_NT)
SMUDGE_EXE=smudge.exe
PLATFORM=windows
/=\\
PKGEXT=zip exe
RC_FILE=$(SMUDGE_BUILD_DIR)/Properties.o
CABAL_FLAGS+=--ghc-options $(RC_FILE)
else
SMUDGE_EXE=smudge
PLATFORM=linux
/=/
PKGEXT=tgz deb
DEBDIR=debian/smudge-$(SMUDGE_VERSION)-linux
endif
define cabal_query
$(shell grep "^$(1):" smudge.cabal | cut -d ':' -f 1 --complement | sed -e 's/^\s*//' -e 's/\s*$$//')
endef
SMUDGE_BUILD_DIR_RAW=$(shell stack path --local-install-root)
SMUDGE_BUILD_DIR=$(subst \,/,$(SMUDGE_BUILD_DIR_RAW))
SMUDGE_RELEASE_SUBDIR=smudge
SMUDGE_RELEASE_STAGE_DIR=$(SMUDGE_BUILD_DIR)/$(SMUDGE_RELEASE_SUBDIR)
SMUDGE_TARGET=$(SMUDGE_BUILD_DIR)/bin/$(SMUDGE_EXE)
SMUDGE_VERSION=$(call cabal_query,version)
POUND=\\\#
COMMA=,

.PHONY: all tags build examples doc \
        release stage package zip tgz exe deb \
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

$(SMUDGE_TARGET): smudge.cabal stack.yaml $(HSFILES) $(RC_FILE)
	stack $(STACK_FLAGS) build $(CABAL_FLAGS)

$(RC_FILE):
	@echo $(POUND)include \<winver.h\>                                      >  $(@:%.o=%.rc)
	@echo VS_VERSION_INFO VERSIONINFO                                       >> $(@:%.o=%.rc)
	@echo   FILEVERSION        $(subst .,$(COMMA),$(SMUDGE_VERSION))        >> $(@:%.o=%.rc)
	@echo   PRODUCTVERSION     $(subst .,$(COMMA),$(SMUDGE_VERSION))        >> $(@:%.o=%.rc)
	@echo BEGIN                                                             >> $(@:%.o=%.rc)
	@echo   BLOCK \"StringFileInfo\"                                        >> $(@:%.o=%.rc)
	@echo   BEGIN                                                           >> $(@:%.o=%.rc)
	@echo     BLOCK \"040904B0\"                                            >> $(@:%.o=%.rc)
	@echo     BEGIN                                                         >> $(@:%.o=%.rc)
	@echo       VALUE \"FileVersion\", \"$(SMUDGE_VERSION)\"                >> $(@:%.o=%.rc)
	@echo       VALUE \"ProductVersion\", \"$(SMUDGE_VERSION)\"             >> $(@:%.o=%.rc)
	@echo       VALUE \"FileDescription\", \"Smudge\"                       >> $(@:%.o=%.rc)
	@echo       VALUE \"ProductName\", \"Smudge\"                           >> $(@:%.o=%.rc)
	@echo       VALUE \"InternalName\", \"Smudge\"                          >> $(@:%.o=%.rc)
	@echo       VALUE \"OriginalFilename\", \"$(SMUDGE_EXE)\"               >> $(@:%.o=%.rc)
	@echo       VALUE \"LegalCopyright\", \"$(call cabal_query,copyright)\" >> $(@:%.o=%.rc)
	@echo       VALUE \"CompanyName\", \"$(call cabal_query,copyright)\"    >> $(@:%.o=%.rc)
	@echo       VALUE \"License\", \"$(call cabal_query,license)\"          >> $(@:%.o=%.rc)
	@echo     END                                                           >> $(@:%.o=%.rc)
	@echo   END                                                             >> $(@:%.o=%.rc)
	@echo   BLOCK \"VarFileInfo\"                                           >> $(@:%.o=%.rc)
	@echo   BEGIN                                                           >> $(@:%.o=%.rc)
	@echo     VALUE \"Translation\", 0x0409, 0x04B0                         >> $(@:%.o=%.rc)
	@echo   END                                                             >> $(@:%.o=%.rc)
	@echo END                                                               >> $(@:%.o=%.rc)
	windres -i $(@:%.o=%.rc) $@

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

$(SMUDGE_BUILD_DIR)/setup.iss: setup.iss.in smudge.cabal
	@echo $(POUND)define MyAppName      \"Smudge\" > $@
	@echo $(POUND)define MyAppVersion   \"$(SMUDGE_VERSION)\" >>$@
	@echo $(POUND)define MyAppPublisher \"$(call cabal_query,copyright)\" >>$@
	@echo $(POUND)define MyAppURL       \"$(call cabal_query,  location)\" >>$@
	@echo $(POUND)define MyOutputDir    \"$(SMUDGE_BUILD_DIR)\" >>$@
	@echo $(POUND)define MySetupDir     \"$(SMUDGE_RELEASE_STAGE_DIR)\" >>$@
	cat $< >>$@

exe: smudge-$(SMUDGE_VERSION)-$(PLATFORM).exe
smudge-$(SMUDGE_VERSION)-windows.exe: $(SMUDGE_BUILD_DIR)/setup.iss stage
	ISCC /Q $(SMUDGE_BUILD_DIR_RAW)\setup.iss
	mv $(SMUDGE_BUILD_DIR)/$@ .

tgz: smudge-$(SMUDGE_VERSION)-$(PLATFORM).tgz
smudge-$(SMUDGE_VERSION)-linux.tgz: stage
	cd $(SMUDGE_BUILD_DIR) && \
	fakeroot tar -czf $@ $(SMUDGE_RELEASE_SUBDIR)
	mv $(SMUDGE_BUILD_DIR)/$@ .

deb: smudge-$(SMUDGE_VERSION)-$(PLATFORM).deb
smudge-$(SMUDGE_VERSION)-linux.deb: stage
	mkdir -p $(DEBDIR)/DEBIAN
	mkdir -p $(DEBDIR)/usr/bin
	mkdir -p $(DEBDIR)/usr/share/doc/smudge/
	cp $(SMUDGE_BUILD_DIR)/smudge/smudge $(DEBDIR)/usr/bin
	chrpath -d $(DEBDIR)/usr/bin/smudge
	cp -r $(SMUDGE_BUILD_DIR)/$(SMUDGE_RELEASE_SUBDIR)/* $(DEBDIR)/usr/share/doc/smudge/
	rm $(DEBDIR)/usr/share/doc/smudge/smudge # No need for extra binary
	chmod -R a+rX $(DEBDIR)/usr/share/doc/smudge/*
	# Note: the copyright file duplicates info from LICENSE.
	cp debian/copyright $(DEBDIR)/usr/share/doc/smudge/
	chmod 755 $(DEBDIR)/usr/bin/smudge
	sed -e "s/__VERSION__/$(SMUDGE_VERSION)/g" -e "s/__ARCH__/`dpkg --print-architecture`/g" debian/control > $(DEBDIR)/DEBIAN/control
	fakeroot dpkg --build $(DEBDIR)
	mv $(DEBDIR).deb . # Whew, puns

newticket:
	cd tickets && ./mkticket.sh "$(title)"

todo:
	@find roadmap/$V | while read -r fn; do find -L tickets/ -xdev -samefile $$fn; done

clean:
	stack clean
	rm -rf TAGS tags
	rm -f *.tgz *.deb
	$(MAKE) -C examples clean
	$(MAKE) -C docs$/tutorial clean
	$(MAKE) -C docs$/definition clean

distclean: clean
	rm -rf .stack-work
	rm -rf $(DEBDIR)
