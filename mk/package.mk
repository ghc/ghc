# -----------------------------------------------------------------------------
# $Id: package.mk,v 1.55 2005/05/13 10:05:33 krasimir Exp $

ifneq "$(PACKAGE)" ""

# -----------------------------------------------------------------------------
# Directory layouts, installation etc.

# Here Windows & Unix differ.  On Windows, the value of $(prefix) is known
# to the compiler, and spliced into package.conf in place of $topdir at
# runtime.
#
# On Unix, we only use absolute paths in package.conf.
#

ifeq "$(Windows)" "YES"

PKG_LIBDIR  = $$topdir
PKG_DATADIR = $$topdir

else

PKG_LIBDIR  = $(libdir)
PKG_DATADIR = $(datadir)

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

PACKAGE_CPP_OPTS += -I$(GHC_INCLUDE_DIR) -Iinclude

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

install::
	$(CPP) $(RAWCPP_FLAGS) -P -DINSTALLING \
	       -DIMPORT_DIR='"$(IMPORT_DIR_INSTALLED)"' \
	       -DLIB_DIR='"$(LIB_DIR_INSTALLED)"' \
	       -DINCLUDE_DIR='"$(INCLUDE_DIR_INSTALLED)"' \
	       -DDATA_DIR='"$(DATA_DIR_INSTALLED)"' \
	       -DHTML_DIR='"$(HTML_DIR_INSTALLED)"' \
	       -DHADDOCK_IFACE='"$(HADDOCK_IFACE_INSTALLED)"' \
	       -I../includes \
	       -x c $(PACKAGE_CPP_OPTS) package.conf.in \
	    | grep -v '^#pragma GCC' \
	    | sed -e 's/""//g' -e 's/:[   ]*,/: /g' \
	    | $(GHC_PKG_INSTALL_PROG) --global-conf $(DESTDIR)$(datadir)/package.conf update - --force

GHC_PKG_INSTALL_PROG = $(FPTOOLS_TOP_ABS)/utils/ghc-pkg/dist-install/build/ghc-pkg/ghc-pkg

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

STAMP_PKG_CONF = $(GHC_DRIVER_DIR)/stamp-pkg-conf-$(PACKAGE)
CLEAN_FILES += $(STAMP_PKG_CONF)

ifneq "$(BootingFromHc)" "YES"
boot all :: $(STAMP_PKG_CONF)
endif

$(STAMP_PKG_CONF) : package.conf.inplace
	$(GHC_PKG_INPLACE) update - --force-files <package.conf.inplace
	@touch $(STAMP_PKG_CONF)

CLEAN_FILES += package.conf.inplace 

endif # $(way) == ""

# -----------------------------------------------------------------------------
# Building the static library libHS<pkg>.a

SRC_HSC2HS_OPTS += -I.

ifneq "$(NO_SET_HC)" "YES"
HC = $(GHC_INPLACE)
BOOTSTRAPPING_PACKAGE_CONF_HC_OPTS =
endif
IGNORE_PACKAGE_FLAG = -package-name  $(PACKAGE)-$(VERSION)

ifeq "$(NON_HS_PACKAGE)" ""
SRC_HC_OPTS	+= $(IGNORE_PACKAGE_FLAG)
SRC_HC_OPTS 	+= $(GhcLibHcOpts)
SRC_HC_OPTS     += $(patsubst %, -package %, $(PACKAGE_DEPS))
endif

#	-XGenerics switches on generation of support code for 
#		derivable type classes.  This is now off by default,
#		but we switch it on for the libraries so that we generate
#		the code in case someone importing wants it.
ifeq "$(NON_HS_PACKAGE)" ""
SRC_HC_OPTS	+= -XGenerics
endif

ifndef LIBRARY
ifeq "$(_way:%_dyn=YES)" "YES"
LIBRARY      	= libHS$(PACKAGE)$(_way:%_dyn=%)-ghc$(ProjectVersion)$(soext)
else
LIBRARY      	= libHS$(PACKAGE)$(_way).a
endif
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

MKDEPENDHS = $(GHC_INPLACE)

SRC_MKDEPENDC_OPTS += $(addprefix -I,$(ALL_DIRS))
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

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
endif # $(LIBRARY) /= ""

# -----------------------------------------------------------------------------
# Doc building with Haddock

ifneq "$(PACKAGE)" ""
ifeq "$(HADDOCK_DOCS)" "YES"

HS_PPS = $(addsuffix .raw-hs, $(basename $(filter-out $(EXCLUDED_HADDOCK_SRCS), $(HS_SRCS)))) $(EXTRA_HADDOCK_SRCS)

HTML_DIR = ../html-docs/$(PACKAGE)
HTML_DOC = $(HTML_DIR)/haddock.css $(HTML_DIR)/haddock.js

ifneq "$(HS_PPS)" ""

CLEAN_FILES += $(HS_PPS) $(addsuffix .tmp, $(HS_SRCS))

html :: $(HTML_DOC)

extraclean :: 
	$(RM) -rf $(HTML_DIR)

$(HTML_DOC) : $(HS_PPS)
	@$(INSTALL_DIR) $(HTML_DIR)
	$(HADDOCK) $(HADDOCK_OPTS) -h -o $(HTML_DIR) $(HS_PPS) \
		--package=$(PACKAGE) \
		--dump-interface=$(PACKAGE).haddock \
		--use-index=../doc-index.html --use-contents=../index.html \
		--source-module=$(PackageSourceURL) \
		$(foreach pkg, $(PACKAGE_DEPS), \
		   --read-interface=../$(pkg),../$(pkg)/$(pkg).haddock)

CLEAN_FILES += $(PACKAGE).haddock

%.raw-hs : %.lhs
	$(HC) $(HC_OPTS) -D__HADDOCK__ -E $< -o $@

%.raw-hs : %.hs
	$(HC) $(HC_OPTS) -D__HADDOCK__ -E $< -o $@

HTML_INSTALL_DIR = $(DESTDIR)$(htmldir)/libraries/html/$(PACKAGE)

install-docs :: $(HTML_DOC)
	@$(INSTALL_DIR) $(HTML_INSTALL_DIR)
	for i in $(HTML_DIR)/*; do \
	  echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(HTML_INSTALL_DIR); \
	  $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(HTML_INSTALL_DIR); \
	done; \
	$(INSTALL_DATA) $(INSTALL_OPTS) $(PACKAGE).haddock $(HTML_INSTALL_DIR); \

endif # HS_PPS
endif # HADDOCK_DOCS
endif # $(PACKAGE) /= ""

# -----------------------------------------------------------------------------

