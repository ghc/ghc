#################################################################################
#
#			fptools/Makefile
#
#		This is the main Makefile for fptools.
#
#################################################################################

TOP=.
include $(TOP)/mk/boilerplate.mk
SRC_DIST_DIR=$(shell pwd)/$(SRC_DIST_NAME)

#
# Totally evil hack to make the setting of SUBDIRS be dependent
# on whether we do `make install' or not. Having a $(ifeq ... ) would
# be preferable..
CURRENT_TARGET = $@
SUBDIRS = $(shell if (test x$(CURRENT_TARGET) = xinstall) ; then echo $(ProjectsToInstall); else echo $(ProjectsToBuild); fi)

#
# Files to include in fptools source distribution
#
SRC_DIST_DIRS += mk docs CONTRIB distrib $(ProjectsToBuild)
SRC_DIST_FILES += configure.in config.guess config.sub configure aclocal.m4 README ANNOUNCE INSTALL Makefile install-sh

#
# Making a binary distribution
#
BIN_DIST_TMPDIR=$(shell pwd)
BIN_DIST_NAME=fptools

#
# list of toplevel `projects' to include in binary distrib.
#
BIN_DIST_DIRS=ghc

binary-dist:: binary-dist-pre

BIN_DIST_TOP= distrib/Makefile-bin.in \
	      distrib/configure-bin.in \
	      README \
	      distrib/INSTALL \
	      ANNOUNCE \
	      ghc/PATCHLEVEL \
	      glafp-utils/mkdirhier/mkdirhier \
	      ghc/RELEASE \
	      install-sh \
	      config.guess \
	      config.sub   \
	      aclocal.m4

binary-dist::
	@for i in $(BIN_DIST_TOP); do \
	  echo cp $$i $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); \
	  cp $$i $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); \
	done;
	@echo "hackily rename some of these chaps.."
	$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile-bin.in $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in 
	$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/configure-bin.in $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/configure.in 
	@echo "Generating a shippable configure script.."
	( cd $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); autoconf )

#
# binary dist'ing the documentation.  
# Which documentation to build/install is hardcoded below.
#

BINDIST_DOCS = docs ghc/docs/users_guide
BINDIST_DOCS_WAYS = html info dvi

binary-dist ::
	@for way in $(BINDIST_DOCS_WAYS); do \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	   for dir in $(BINDIST_DOCS); do \
	     echo Making $$way documentation in $$dir && \
	     $(MAKE) -C $$dir --no-print-directory $(MFLAGS) $$way && \
	     echo cp -f $$dir/*.$$way $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way/$(GhcProjectNameShort)-$(GhcProjectVersion) && \
	     cp -f $$dir/*.$$way $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way/$(GhcProjectNameShort)-$(GhcProjectVersion) && \
	     echo "Done."; \
	   done; \
	done

#
# binary dist'ing hslibs/, hackily.
#
binary-dist ::
	@echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/hslibs;
	@$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/hslibs;
	$(MAKE) -C hslibs $(MFLAGS) install \
		BIN_DIST=1 BIN_DIST_NAME=$(BIN_DIST_NAME) \
		prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion) \
		libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/hslibs \
		libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/hslibs \
		datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share/ghc-$(GhcProjectVersion) ;

# Even more of a hack, but I'm too tired to fix this up right now..
BIN_DIST_SCRIPTS_NEEDING_RENAMING=ghc stat2resid hstags mkdependHS

binary-dist::
	@for i in $(BIN_DIST_SCRIPTS_NEEDING_RENAMING); do \
	     echo "Renaming $$i to $$i.prl"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/$$i.prl; \
	done
	@echo "Renaming hscpp to hscpp.prl"
	@$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/hscpp  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/hscpp.prl

# binary-disting happy, hackily again

binary-dist ::
	@echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/happy;
	@$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/happy;
	$(MAKE) -C happy $(MFLAGS) install \
		BIN_DIST=1 BIN_DIST_NAME=$(BIN_DIST_NAME) \
		prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion) \
		libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion) \
		libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion) \
		datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share/ghc-$(GhcProjectVersion) ;
	@echo "Renaming happy to happy.sh"
	@$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/happy  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(GhcProjectVersion)/happy.sh

dist :: dist-pre
include $(TOP)/mk/target.mk
dist :: dist-post

binary-dist::
	@echo "Mechanical and super-natty! Inspect the result and *if* happy; freeze, sell and get some sleep!"
