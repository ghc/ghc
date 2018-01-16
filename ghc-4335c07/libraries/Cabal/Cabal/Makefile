
VERSION=2.1.0.0

#KIND=devel
KIND=rc
#KIND=cabal-latest

PREFIX=/usr/local
HC=ghc
GHCFLAGS=-Wall -threaded
SSH_USER=$(USER)

all: build

# build the library itself

SOURCES=Distribution/*.hs Distribution/Simple/*.hs \
	Distribution/PackageDescription/*.hs Distribution/Simple/GHC/*.hs \
	Distribution/Simple/Build/*.hs Distribution/Compat/*.hs \
	Distribution/Simple/Program/*.hs
CONFIG_STAMP=dist/setup-config
BUILD_STAMP=dist/build/libHSCabal-$(VERSION).a
HADDOCK_STAMP=dist/doc/html/Cabal/index.html
USERGUIDE_STAMP=dist/doc/users-guide/index.html
SDIST_STAMP=dist/Cabal-$(VERSION).tar.gz
DISTLOC=dist/release
DIST_STAMP=$(DISTLOC)/Cabal-$(VERSION).tar.gz
SPHINXCMD:=$(shell command -v sphinx-build2 2> /dev/null)
ifndef SPHINXCMD
	SPHINXCMD:=$(shell command -v sphinx-build 2> /dev/null)
endif
ifndef SPHINXCMD
	$(error "needs sphinx-build")
endif


COMMA=,

setup: $(SOURCES) Setup.hs
	-mkdir -p dist/setup
	$(HC) $(GHCFLAGS) --make -i. -odir dist/setup -hidir dist/setup Setup.hs \
		-o setup

$(CONFIG_STAMP): setup Cabal.cabal
	./setup configure --with-compiler=$(HC) --prefix=$(PREFIX)

build: $(BUILD_STAMP)
$(BUILD_STAMP): $(CONFIG_STAMP) $(SOURCES)
	./setup build

install: $(BUILD_STAMP)
	./setup install

# documentation...

haddock: $(HADDOCK_STAMP)
$(HADDOCK_STAMP) : $(CONFIG_STAMP) $(BUILD_STAMP)
	./setup haddock

SPHINX_HTML_OUTDIR=dist/doc/users-guide

users-guide: $(USERGUIDE_STAMP)
$(USERGUIDE_STAMP) : doc/*.rst
	mkdir -p $(SPHINX_HTML_OUTDIR)
	$(SPHINXCMD) doc $(SPHINX_HTML_OUTDIR)

docs: haddock users-guide

clean:
	rm -rf dist/
	rm -f setup

# testing...

moduleTest: tests/ModuleTest.hs tests/PackageDescriptionTests.hs
	mkdir -p dist/test
	$(HC) --make -Wall -DDEBUG -odir dist/test -hidir dist/test \
		-itests tests/ModuleTest.hs -o moduleTest

#tests: moduleTest clean
#	cd tests/A && $(MAKE) clean
#	cd tests/HUnit-1.0 && $(MAKE) clean
#	cd tests/A && $(MAKE)
#	cd tests/HUnit-1.0 && $(MAKE)

#check:
#	rm -f moduleTest
#	$(MAKE) moduleTest
#	./moduleTest

# distribution...

$(SDIST_STAMP) : $(BUILD_STAMP)
	./setup sdist

dist: $(DIST_STAMP)
$(DIST_STAMP) : $(HADDOCK_STAMP) $(USERGUIDE_STAMP) $(SDIST_STAMP)
	rm -rf $(DISTLOC)
	mkdir $(DISTLOC)
	tar -xzf $(SDIST_STAMP) -C $(DISTLOC)/
	mkdir -p $(DISTLOC)/Cabal-$(VERSION)/doc
	cp -r dist/doc/html $(DISTLOC)/Cabal-$(VERSION)/doc/API
	cp -r dist/doc/users-guide $(DISTLOC)/Cabal-$(VERSION)/doc/
	cp changelog $(DISTLOC)/Cabal-$(VERSION)/
	tar --format=ustar -C $(DISTLOC) -czf $(DISTLOC)/Cabal-$(VERSION).tar.gz Cabal-$(VERSION)
	mkdir $(DISTLOC)/doc
	mv $(DISTLOC)/Cabal-$(VERSION)/doc/users-guide $(DISTLOC)/doc
	mv $(DISTLOC)/Cabal-$(VERSION)/doc/API $(DISTLOC)/doc
	mv $(DISTLOC)/Cabal-$(VERSION)/changelog $(DISTLOC)/
	rm -r $(DISTLOC)/Cabal-$(VERSION)/
	@echo "Cabal tarball built: $(DIST_STAMP)"
	@echo "Release fileset prepared: $(DISTLOC)/"

# Out of date, use SFTP
#release: $(DIST_STAMP)
#	scp -r $(DISTLOC) $(SSH_USER)@haskell.org:/home/web/haskell.org/cabal/release/cabal-$(VERSION)
#	ssh $(SSH_USER)@haskell.org 'cd /home/web/haskell.org/cabal/release && rm -f $(KIND) && ln -s cabal-$(VERSION) $(KIND)'

# tags...

TAGSSRCDIRS = Distribution Language
tags TAGS: $(SOURCES)
	find $(TAGSSRCDIRS) -name \*.\*hs | xargs hasktags
