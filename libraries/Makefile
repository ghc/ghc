# -----------------------------------------------------------------------------

TOP=.
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

# The Cabal package has a GNUmakefile for standalone building, but we
# want the Makefile.
MFLAGS += -f Makefile

SUBDIRS = base haskell98 template-haskell readline \
	  regex-base regex-posix regex-compat parsec Cabal

ifeq "$(GhcLibsWithUnix)" "YES"
SUBDIRS += unix
endif
ifeq "$(Windows)" "YES"
SUBDIRS += $(wildcard Win32)
endif

# Set GhcBootLibs=YES from the command line to work with just the libraries
# needed to bootstrap GHC.
ifneq "$(GhcBootLibs)" "YES"
SUBDIRS += $(wildcard haskell-src)
SUBDIRS += $(wildcard html)
SUBDIRS += $(wildcard network)
SUBDIRS += $(wildcard QuickCheck)
SUBDIRS += $(wildcard HUnit)
SUBDIRS += $(wildcard mtl)
SUBDIRS += $(wildcard fgl)
SUBDIRS += $(wildcard X11)
SUBDIRS += $(wildcard time)
ifeq "$(Windows)" "NO"
# HGL is not working on Win32, so omit it for now.  Better not to ship it
# at all than to ship a broken version.
SUBDIRS += $(wildcard HGL)
endif
SUBDIRS += $(wildcard OpenGL)
SUBDIRS += $(wildcard GLUT)
SUBDIRS += $(wildcard OpenAL)
SUBDIRS += $(wildcard ALUT)
SUBDIRS += $(wildcard stm)
SUBDIRS += $(wildcard xhtml)
SUBDIRS += $(wildcard cgi)
ifeq "$(GhcLibsWithObjectIO)" "YES"
SUBDIRS += $(wildcard ObjectIO)
endif
endif

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += config.cache config.status

include $(TOP)/mk/target.mk

# -----------------------------------------------------------------------------
# Generating the combined contents/index pages for the library docs

ifneq "$(NO_HADDOCK_DOCS)" "YES"

HTML_DIR = html-docs

extraclean ::
	$(RM) -rf $(HTML_DIR)

# ATTENTION, incomprehensible shell stuff ahead: Automagically create the
# prologue for the combined index via a header, the package prologues (in
# alphabetical order of the packages) and a footer. Not very nice, but much
# better than redundancy or a strong coupling with the packages.
libraries.txt: libraries-header.txt libraries-footer.txt $(foreach f,package.conf.in prologue.txt,$(addsuffix /$(f),$(SUBDIRS)))
	$(RM) $@
	( cat libraries-header.txt ; echo ; \
	for i in `for j in $(SUBDIRS) ; do echo $$j ; done | $(SORT) -f` ; do \
	   if test -f $$i/$$i.haddock; then \
	      echo "[@$$i@]"; \
	      grep -v '^ *$$' $$i/prologue.txt; \
	      echo; \
	   fi; \
	done ; \
	cat libraries-footer.txt ; echo ) > $@

CLEAN_FILES += libraries.txt

$(HTML_DIR)/index.html : libraries.txt
	@$(INSTALL_DIR) $(HTML_DIR)
	$(HADDOCK) --gen-index --gen-contents -o $(HTML_DIR) \
		$(HADDOCK_OPTS) \
		-t "Haskell Hierarchical Libraries" \
		-p libraries.txt \
		$(foreach pkg, $(filter-out haskell98,$(SUBDIRS)), \
		   $(foreach p, $(wildcard $(pkg)/$(pkg).haddock), \
		      --read-interface=$(pkg),$(pkg)/$(pkg).haddock))

html :: $(HTML_DIR)/index.html

libraries.HxS : libraries.txt
	@$(INSTALL_DIR) $(HTML_DIR)
	$(HADDOCK) --gen-index --gen-contents -o $(HTML_DIR) \
		$(HADDOCK_OPTS) \
		-t "Haskell Hierarchical Libraries" \
		-p libraries.txt \
		-k libraries \
		--html-help=mshelp2 \
		$(foreach pkg, $(filter-out haskell98,$(SUBDIRS)), \
		   $(foreach p, $(wildcard $(pkg)/$(pkg).haddock), \
		      --read-interface=$(pkg),$(pkg)/$(pkg).haddock))
	( cd $(HTML_DIR) && if Hxcomp -p libraries.HxC -o ../$@ ; then false ; else true ; fi ) || true

libraries.chm : libraries.txt
	@$(INSTALL_DIR) $(HTML_DIR)
	$(HADDOCK) --gen-index --gen-contents -o $(HTML_DIR) \
		$(HADDOCK_OPTS) \
		-t "Haskell Hierarchical Libraries" \
		-p libraries.txt \
		-k libraries \
		--html-help=mshelp \
		$(foreach pkg, $(filter-out haskell98,$(SUBDIRS)), \
		   $(foreach p, $(wildcard $(pkg)/$(pkg).haddock), \
		      --read-interface=$(pkg),$(pkg)/$(pkg).haddock))
	( cd $(HTML_DIR) && if hhc libraries.hhp ; then false ; else true ; fi && mv libraries.chm .. ) || true

HxS :: libraries.HxS
chm :: libraries.chm

install-docs :: $(HTML_DIR)/index.html
	$(INSTALL_DIR) $(datadir)/html/libraries; \
	for i in $(HTML_DIR)/*; do \
		if test -f $$i; then \
		  echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir)/html/libraries; \
		  $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir)/html/libraries; \
		fi; \
	done
	@for i in $(filter html chm HxS,$(XMLDocWays)); do \
		if [ $$i != "html" ]; then \
			$(INSTALL_DIR) $(datadir)/doc; \
			echo $(INSTALL_DATA) $(INSTALL_OPTS) libraries`echo .$$i | sed s/\.html-no-chunks/.html/` $(datadir)/doc; \
			$(INSTALL_DATA) $(INSTALL_OPTS) libraries`echo .$$i | sed s/\.html-no-chunks/.html/` $(datadir)/doc; \
		fi; \
		if [ $$i = "html-no-chunks" ]; then \
			echo $(CP) $(FPTOOLS_CSS_ABS) $(datadir); \
			$(CP) $(FPTOOLS_CSS_ABS) $(datadir); \
		fi \
	done
endif # NO_HADDOCK_DOCS
