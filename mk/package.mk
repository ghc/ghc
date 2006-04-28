# -----------------------------------------------------------------------------
# $Id: package.mk,v 1.55 2005/05/13 10:05:33 krasimir Exp $

ifneq "$(PACKAGE)" ""

# FIXME: does anyone do this any more?
STANDALONE_PACKAGE = NO

# -----------------------------------------------------------------------------
# Directory layouts, installation etc.

# Here Windows & Unix differ.  On Windows, the value of $(prefix) is known
# to the compiler, and spliced into package.conf in place of $topdir at
# runtime.
#
# On Unix, we only use absolute paths in package.conf, except that when
# building a binary distribution we use $libdir and $datadir in package.conf
# which are then replaced by the correct values at install time.
#

ifeq "$(Windows)" "YES"

PKG_LIBDIR  = $$topdir
PKG_DATADIR = $$topdir

else

ifeq "$(BIN_DIST)" ""
PKG_LIBDIR  = $(libdir)
PKG_DATADIR = $(datadir)
else
PKG_LIBDIR  = $$libdir
PKG_DATADIR = $$datadir
endif

endif # Unix

IMPORT_DIR_INSTALLED = $(PKG_LIBDIR)/imports
IMPORT_DIR_INPLACE   = $(FPTOOLS_TOP_ABS)/libraries/$(PACKAGE)

INCLUDE_DIR_INSTALLED = $(PKG_LIBDIR)/include
INCLUDE_DIR_INPLACE   = $(FPTOOLS_TOP_ABS)/libraries/$(PACKAGE)/include

LIB_DIR_INSTALLED    = $(PKG_LIBDIR)
LIB_DIR_INPLACE	     = $(FPTOOLS_TOP_ABS)/libraries/$(PACKAGE)

DATA_DIR_INSTALLED   = $(PKG_DATADIR)
DATA_DIR_INPLACE     = $(FPTOOLS_TOP_ABS)/libraries/$(PACKAGE)

HTML_DIR_INPLACE     = $(FPTOOLS_TOP_ABS)/libraries/$(PACKAGE)/html
HTML_DIR_INSTALLED   = $(PKG_DATADIR)/html/libraries/$(PACKAGE)

HADDOCK_IFACE_INPLACE   = $(HTML_DIR_INPLACE)/$(PACKAGE).haddock
HADDOCK_IFACE_INSTALLED = $(HTML_DIR_INSTALLED)/$(PACKAGE).haddock

# -----------------------------------------------------------------------------
# Build the package configuration file and tell the compiler about it.

# We want to build two versions of the package configuration: one for use
# in the 

ifeq "$(way)" ""

ifeq "$(STANDALONE_PACKAGE)" "NO"
PACKAGE_CPP_OPTS += -I$(GHC_INCLUDE_DIR) -Iinclude
else
PACKAGE_CPP_OPTS += -Iinclude
endif

PACKAGE_CPP_OPTS += -DPACKAGE=${PACKAGE}
PACKAGE_CPP_OPTS += -DVERSION=${VERSION}

PACKAGE_CPP_OPTS += -DPKG_LIBDIR='"$(PKG_LIBDIR)"'
PACKAGE_CPP_OPTS += -DPKG_DATADIR='"$(PKG_DATADIR)"'

package.conf.inplace   : package.conf.in
	$(CPP) $(RAWCPP_FLAGS) -P \
		-DIMPORT_DIR='"$(IMPORT_DIR_INPLACE)"' \
		-DLIB_DIR='"$(LIB_DIR_INPLACE)"' \
		-DINCLUDE_DIR='"$(INCLUDE_DIR_INPLACE)"' \
		-DDATA_DIR='"$(DATA_DIR_INPLACE)"' \
		-DHTML_DIR='"$(HTML_DIR_INPLACE)"' \
		-DHADDOCK_IFACE='"$(HADDOCK_IFACE_INPLACE)"' \
		-DFPTOOLS_TOP_ABS='"${FPTOOLS_TOP_ABS}"' \
		-x c $(PACKAGE_CPP_OPTS) $< | \
	grep -v '^#pragma GCC' | \
	sed -e 's/""//g' -e 's/:[ 	]*,/: /g' >$@

package.conf.installed : package.conf.in
	$(CPP) $(RAWCPP_FLAGS) -P -DINSTALLING \
		-DIMPORT_DIR='"$(IMPORT_DIR_INSTALLED)"' \
		-DLIB_DIR='"$(LIB_DIR_INSTALLED)"' \
		-DINCLUDE_DIR='"$(INCLUDE_DIR_INSTALLED)"' \
		-DDATA_DIR='"$(DATA_DIR_INSTALLED)"' \
		-DHTML_DIR='"$(HTML_DIR_INSTALLED)"' \
		-DHADDOCK_IFACE='"$(HADDOCK_IFACE_INSTALLED)"' \
		 -x c $(PACKAGE_CPP_OPTS) $< | \
	grep -v '^#pragma GCC' | \
	sed -e 's/""//g' -e 's/:[ 	]*,/: /g' >$@

# we could be more accurate here and add a dependency on
# driver/package.conf, but that doesn't work too well because of
# make's limited accuracy with modification times: when doing 'make
# boot' in multiple packages, make won't detect that the package
# configuration needs updating if it was updated already in the last
# second.
#
# The stamp file goes in $(GHC_DRIVER_DIR), so that if someone happens
# to 'make clean' in ghc without cleaning in libraries too, the packages
# will be correctly re-installed.
#
ifeq "$(STANDALONE_PACKAGE)" "NO"

STAMP_PKG_CONF = $(GHC_DRIVER_DIR)/stamp-pkg-conf-$(PACKAGE)
CLEAN_FILES += $(STAMP_PKG_CONF)

ifneq "$(BootingFromHc)" "YES"
boot all :: $(STAMP_PKG_CONF)
endif

$(STAMP_PKG_CONF) : package.conf.inplace package.conf.installed
	$(GHC_PKG_INPLACE) --force --update-package <package.conf.inplace
	$(GHC_PKG_INPLACE)  -f $(GHC_DRIVER_DIR)/package.conf --force --update-package <package.conf.installed
	@touch $(STAMP_PKG_CONF)

CLEAN_FILES += package.conf.installed package.conf.inplace 

else # $(STANDALONE_PACKAGE) == "YES"

PACKAGE_CPP_OPTS += -DPACKAGE_DEPS='$(subst " ","$(comma) ",$(patsubst %,"%",$(PACKAGE_DEPS)))'
PACKAGE_CPP_OPTS += -DLIBRARY=\"HS$(PACKAGE)\"
PACKAGE_CPP_OPTS += -DLIBDIR=\"$(libdir)\"

# Let the package configuration file refer to $(libdir) as
# ${pkglibdir}.  Note we can't use ${libdir} because ghc-pkg already
# redefines it to point to GHC's libdir (bug or feature?).
#
install :: package.conf.installed
	$(GHC_PKG) --force --update-package <package.conf.installed

# Invoke this rule by hand in order to use the package in-place
install-inplace-pkg : package.conf.inplace
	$(GHC_PKG) --force --update-package <package.conf.inplace

endif # $(STANDALONE_PACKAGE)

endif # $(way) == ""

# -----------------------------------------------------------------------------
# Building the static library libHS<pkg>.a

SRC_HSC2HS_OPTS += -I.

ifeq "$(STANDALONE_PACKAGE)" "NO"
HC = $(GHC_INPLACE)
IGNORE_PACKAGE_FLAG = -ignore-package
else
# Only use -ignore-package if supported by HC; i.e., ghc-6.3 and later.
# (Don't like the use of slow $(shell ..) in Makefiles, but can't see a way around it here.)
ifeq "$(strip $(GHC))" ""
IGNORE_PACKAGE_FLAG = -ignore-package
else
# Making the assumption here that standalone packages will be using mk/config.mk:GHC
IGNORE_PACKAGE_FLAG = $(shell if (test $(GhcCanonVersion) -ge 603); then echo "-ignore-package"; else echo "-package-name"; fi)
endif 
endif # STANDALONE_PACKAGE

ifeq "$(NON_HS_PACKAGE)" ""
SRC_HC_OPTS	+= $(IGNORE_PACKAGE_FLAG) $(PACKAGE)
SRC_HC_OPTS 	+= $(GhcLibHcOpts)
SRC_HC_OPTS     += $(patsubst %, -package %, $(PACKAGE_DEPS))
endif

#	-fgenerics switches on generation of support code for 
#		derivable type classes.  This is now off by default,
#		but we switch it on for the libraries so that we generate
#		the code in case someone importing wants it.
ifeq "$(NON_HS_PACKAGE)" ""
SRC_HC_OPTS	+= -fgenerics
endif

ifndef LIBRARY
LIBRARY      	= libHS$(PACKAGE)$(_way).a
endif

ifeq "$(WAYS)" ""
WAYS = $(GhcLibWays)
endif

all :: $(LIBRARY)

# POSSIBLE alternative version using --make:
#
# lib : $(HS_SRCS)
# 	$(GHC_INPLACE) $(HC_OPTS) --make $(HS_SRCS)
# 
# $(LIBNAME) : lib
# 	$(RM) $@
# 	$(AR) $(AR_OPTS) $@ $(HS_OBJS)
# 	$(RANLIB) $@
# 
# %.o : %.hs
# 	$(GHC_INPLACE) $(HC_OPTS) --make $<
# %.o : %.lhs
# 	$(GHC_INPLACE) $(HC_OPTS) --make $<

# -----------------------------------------------------------------------------
# Installation; need to install .hi files as well as libraries

ifeq "$(DLLized)" "YES"
INSTALL_PROGS += $(DLL_NAME)
INSTALL_LIBS += $(patsubst %.a,%_imp.a, $(LIBRARY))
endif

# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..

ifeq "$(HIERARCHICAL_LIB)" "YES"
ifacedir = $(libdir)/imports
else
ifacedir = $(libdir)/hslibs-imports/$(PACKAGE)
endif

# If the lib consists of a hierachy of modules, we must retain the directory
# structure when we install the interfaces.
ifeq "$(HIERARCHICAL_LIB)" "YES"
INSTALL_IFACES_WITH_DIRS += $(HS_IFACES)
ifneq "$(ALL_DIRS)" ""
install ::
	@for i in $(ALL_DIRS); do \
		$(INSTALL_DIR) $(ifacedir)/$$i; \
	done
endif
else
INSTALL_IFACES += $(HS_IFACES)
endif

# -----------------------------------------------------------------------------
# Dependencies

ifeq "$(STANDALONE_PACKAGE)" "NO"
MKDEPENDHS = $(GHC_INPLACE)
endif

SRC_MKDEPENDC_OPTS += $(addprefix -I,$(ALL_DIRS))

ifeq "$(STANDALONE_PACKAGE)" "NO"
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)
endif

endif # $(PACKAGE) != ""

#--------------------------------------------------------------
# Installation

ifneq "$(NO_INSTALL_LIBRARY)" "YES"
INSTALL_LIBS  += $(LIBRARY) $(GHCI_LIBRARY)
endif

#--------------------------------------------------------------
# Building dynamically-linkable libraries for GHCi
#
# Build $(GHCI_LIBRARY) whenever we build $(LIBRARY)
#
# Why?  GHCi can only link .o files (at the moment), not .a files
# so we have to build libFoo.o as well as libFoo.a
#
# Furthermore, GHCi currently never loads 
# profiling libraries (or other non-std ways)

ifneq "$(LIBRARY)" ""

ifeq "$(way)" ""
ifeq "$(GhcWithInterpreter)" "YES"

ifndef GHCI_LIBRARY
GHCI_LIBRARY = $(patsubst lib%.a,%.o,$(LIBRARY))
endif

ifneq "$(NO_INSTALL_LIBRARY)" "YES"
INSTALL_LIBS += $(GHCI_LIBRARY)
endif

CLEAN_FILES  += $(GHCI_LIBRARY)

all :: $(GHCI_LIBRARY)

ifneq "$(DONT_WANT_STD_GHCI_LIB_RULE)" "YES"
# If you don't want to build GHCI_LIBRARY the 'standard' way,
# set DONT_WANT_STD_GHCI_LIB_RULE to YES. The Prelude and
# hslibs/Win32 uses this 'feature', which will go away soon
# when we can use a "fixed" ld.
#
$(GHCI_LIBRARY) : $(LIBOBJS)
	$(LD) -r $(LD_X) -o $@ $(STUBOBJS) $(LIBOBJS)

endif # DONT_WANT_STD_GHCI_LIB_RULE
endif # GhcWithInterpreter
endif # way

ifeq "$(GhcBuildDylibs)" "YES"

    # Build dynamic libraries.
    # Currently, this is a hack. Anyone, PLEASE clean it up.

    # For now, we pretend that there are two operating systems in the world;
    # Darwin, and Everything Else. Furthermore, we pretend that Everything Else
    # behaves like Linux.
    
ifeq "$(darwin_TARGET_OS)" "1"
    # Darwin: Shared libraries end in .dylib
DYLD_LIBRARY = $(patsubst %.a,%_dyn.dylib,$(LIBRARY))

    # About the options used for Darwin:
    # -dynamiclib
    #   Apple's way of saying -shared
    # -flat_namespace -undefined suppress:
    #   Without these options, we'd have to specify the correct dependencies
    #   for each of the dylibs. Twolevel namespaces are in general a good thing
    #   (they make things more robust), so we should fix this sooner or later.
    # -install_name
    #   Causes the dynamic linker to ignore the DYLD_LIBRARY_PATH when loading
    #   this lib and instead look for it at its absolute path.
    #   When installing the .dylibs (see target.mk), we'll change that path to
    #   point to the place they are installed.
    #   Note: I'm not yet sure about this, but I think it will be convenient for
    #         users not to have to set up DYLD_LIBRARY_PATH to point to the GHC
    #         library dir. -- Wolfgang

$(DYLD_LIBRARY) : $(LIBOBJS) $(STUBOBJS)
	$(CC) -dynamiclib -o $@ $(STUBOBJS) $(LIBOBJS) -flat_namespace -undefined suppress -install_name `pwd`/$@
else
DYLD_LIBRARY = $(patsubst %.a,%_dyn.so,$(LIBRARY))

$(DYLD_LIBRARY) : $(LIBOBJS) $(STUBOBJS)
	$(CC) -shared -o $@ $(STUBOBJS) $(LIBOBJS)
endif

ifneq "$(NO_INSTALL_LIBRARY)" "YES"
INSTALL_LIBS += $(DYLD_LIBRARY)
endif

CLEAN_FILES += $(DYLD_LIBRARY)

all :: $(DYLD_LIBRARY)

endif # $(GhcBuildDylibs) == "YES"

endif # $(LIBRARY) /= ""

# -----------------------------------------------------------------------------
# Doc building with Haddock

ifneq "$(PACKAGE)" ""
ifneq "$(NO_HADDOCK_DOCS)" "YES"

HS_PPS = $(addsuffix .raw-hs, $(basename $(filter-out $(EXCLUDED_HADDOCK_SRCS), $(HS_SRCS)))) $(EXTRA_HADDOCK_SRCS)

HTML_DIR = ../html/$(PACKAGE)
HTML_DOC = $(HTML_DIR)/haddock.css $(HTML_DIR)/haddock.js

ifneq "$(HS_PPS)" ""

CLEAN_FILES += $(HS_PPS) $(addsuffix .tmp, $(HS_SRCS))

ifeq "$(HADDOCK)" ""
html ::
	@echo Haddock must be installed in order to build HTML library documentation.
	@echo Please install Haddock and re-configure.
	@exit 1
endif

html :: $(HTML_DOC)

extraclean :: 
	$(RM) -rf $(HTML_DIR)

$(HTML_DOC) : $(HS_PPS)
	@$(INSTALL_DIR) $(HTML_DIR)
	$(HADDOCK) $(HADDOCK_OPTS) -h -o $(HTML_DIR) $(HS_PPS) \
		--package=$(PACKAGE) \
		--dump-interface=$(PACKAGE).haddock \
		--use-index=../doc-index.html --use-contents=../index.html \
		$(foreach pkg, $(PACKAGE_DEPS), \
		   --read-interface=../$(pkg),../$(pkg)/$(pkg).haddock)

CLEAN_FILES += $(PACKAGE).haddock

%.raw-hs : %.lhs
	$(HC) $(HC_OPTS) -D__HADDOCK__ -E -optP-P $< -o $@

%.raw-hs : %.hs
	$(HC) $(HC_OPTS) -D__HADDOCK__ -E -optP-P $< -o $@

HTML_INSTALL_DIR = $(datadir)/html/libraries/$(PACKAGE)
#  NOT the same as HTML_DIR_INSTALLED when BIN_DIST is on

install-docs :: $(HTML_DOC)
	@$(INSTALL_DIR) $(HTML_INSTALL_DIR)
	for i in $(HTML_DIR)/*; do \
	  echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(HTML_INSTALL_DIR); \
	  $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(HTML_INSTALL_DIR); \
	done; \
	$(INSTALL_DATA) $(INSTALL_OPTS) $(PACKAGE).haddock $(HTML_INSTALL_DIR); \

endif # HS_PPS
endif # NO_HADDOCK_DOCS
endif # $(PACKAGE) /= ""

# -----------------------------------------------------------------------------

