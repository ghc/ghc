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
SRC_DIST_FILES += configure.in config.guess config.sub configure aclocal.m4 README ANNOUNCE ANNOUNCE-$(GhcProjectVersion) NEWS INSTALL Makefile install-sh


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
	      config.sub

binary-dist::
	@for i in $(BIN_DIST_TOP); do \
	  echo cp $$i $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); \
	  cp $$i $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); \
	done;
	@echo "hackily rename some of these chaps.."
	$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile-bin.in $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in 
	$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/configure-bin.in $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/configure.in 
	@echo "Generating a shippable configure script.."
	#-(cd $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); autoconf )

#
# Creating and copying the documentation into the bin-dist tree.
# (this tries to be oh-so-general about copying docs, but isn't really
# suited for anything other than ghc/)
#
# Needless to say, the rule below could be cleaned up somewhat.
#
binary-dist::
	@$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/html/$(GhcProjectNameShort)-$(GhcProjectVersion)
	@$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/dvi/$(GhcProjectNameShort)-$(GhcProjectVersion)
	@$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/info/$(GhcProjectNameShort)-$(GhcProjectVersion)
	@echo "Making html documentation.."
	@echo "For fptools.."
	#$(MAKE) -C docs --no-print-directory $(MFLAGS) html
	#cp -f docs/html/* docs/*.html $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/html/$(GhcProjectNameShort)-$(GhcProjectVersion)
	#@echo "Done."
	#@for i in $(BIN_DIST_DIRS); do \
	#   echo "For $$i.."; \
	#   echo cp -f $$i/docs/users_guide/html/* $$i/docs/users_guide/*.html $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/html/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   cp -f $$i/docs/users_guide/html/* $$i/docs/users_guide/*.html $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/html/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   echo cp -f $$i/docs/html/* $$i/docs/*.html $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/html/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   cp -f $$i/docs/html/* $$i/docs/*.html $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/html/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   echo "Done."; \
	#done
	##@echo "Making dvi files.."
	#@echo "For fptools.."
	#-$(MAKE) -C docs --no-print-directory $(MFLAGS) dvi
	#-cp -f docs/*.dvi  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/dvi/$(GhcProjectNameShort)-$(GhcProjectVersion)
	#@echo "Done."
	#@for i in $(BIN_DIST_DIRS); do \
	#   echo "For $$i.."; \
	#   echo cp -f $$i/docs/users_guide/*.dvi $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/dvi/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   cp -f $$i/docs/users_guide/*.dvi $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/dvi/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   echo cp -f $$i/docs/*.dvi $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/dvi/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   cp -f $$i/docs/*.dvi $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/dvi/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   echo "Done."; \
	#done
	#@echo "Making info files.."
	#@echo "For fptools.."
	#-$(MAKE) -C docs --no-print-directory $(MFLAGS) info
	#-cp -f docs/*.info $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/info/$(GhcProjectNameShort)-$(GhcProjectVersion)/
	#@echo "Done."
	#@for i in $(BIN_DIST_DIRS); do \
	#   echo "For $$i docs.."; \
	#   echo cp -f $$i/docs/users_guide/*.info  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/info/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   cp -f $$i/docs/users_guide/*.info $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/info/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   echo cp -f $$i/docs/*.info $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/info/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   cp -f $$i/docs/*.info $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/info/$(GhcProjectNameShort)-$(GhcProjectVersion); \
	#   echo "Done."; \
	#done

#
# binary dist'ing hslibs/, hackily.
#
binary-dist ::
	@echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(ProjectVersion)/hslibs;
	@$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(ProjectVersion)/hslibs;
	$(MAKE) -C hslibs $(MFLAGS) install \
		BIN_DIST=1 BIN_DIST_NAME=$(BIN_DIST_NAME) \
		prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(ProjectVersion) \
		libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(ProjectVersion)/hslibs \
		libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(ProjectVersion)/hslibs \
		datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share/ghc-$(ProjectVersion) ;

# Even more of a hack, but I'm too tired to fix this up right now..
BIN_DIST_SCRIPTS_NEEDING_RENAMING=ghc stat2resid hstags mkdependHS

binary-dist::
	@for i in $(BIN_DIST_SCRIPTS_NEEDING_RENAMING); do \
	     echo "Renaming $$i to $$i.prl"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(ProjectVersion)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/ghc-$(ProjectVersion)/$$i.prl; \
	done
	@echo "Renaming hscpp to hscpp.prl"
	@$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(ProjectVersion)/hscpp  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/ghc-$(ProjectVersion)/hscpp.prl

dist :: dist-pre
include $(TOP)/mk/target.mk
dist :: dist-post

binary-dist::
	@echo "Mechanical and super-natty! Inspect the result and *if* happy; freeze, sell and get some sleep!"
