# -----------------------------------------------------------------------------
# $Id: package.mk,v 1.31 2003/11/11 11:50:55 simonmar Exp $

ifneq "$(PACKAGE)" ""

ifeq "$(ProjectNameShort)" "ghc"
STANDALONE_PACKAGE = NO
else
STANDALONE_PACKAGE = YES
endif

# -----------------------------------------------------------------------------
# Build the package configuration file and tell the compiler about it.

ifeq "$(way)" ""

ifeq "$(STANDALONE_PACKAGE)" "NO"
PKGCONF_CPP_EXTRA_OPTS = -I$(GHC_INCLUDE_DIR)
else
PKGCONF_CPP_EXTRA_OPTS =
endif

ifeq "$(STANDALONE_PACKAGE)" "NO"
package.conf.inplace   : package.conf.in
	$(CPP) $(RAWCPP_FLAGS) $(PKGCONF_CPP_EXTRA_OPTS) -x c $(PACKAGE_CPP_OPTS) $< \
		| sed 's/^#.*$$//g' >$@

package.conf.installed : package.conf.in
	$(CPP) $(RAWCPP_FLAGS) $(PKGCONF_CPP_EXTRA_OPTS) -DINSTALLING -x c $(PACKAGE_CPP_OPTS) $< \
		| sed 's/^#.*$$//g' >$@

endif

# we could be more accurate here and add a dependency on
# ghc/driver/package.conf, but that doesn't work too well because of
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
	$(GHC_PKG_INPLACE) --update-package <package.conf.inplace
	$(GHC_PKG_INPLACE)  -f $(GHC_DRIVER_DIR)/package.conf --update-package <package.conf.installed
	@touch $(STAMP_PKG_CONF)

CLEAN_FILES += package.conf.installed package.conf.inplace 

else # $(STANDALONE_PACKAGE) == "YES"

PACKAGE_CPP_OPTS += -DPACKAGE=\"${PACKAGE}\"
PACKAGE_CPP_OPTS += -DPACKAGE_DEPS='$(patsubst %,"%"$(comma),$(PACKAGE_DEPS))'
PACKAGE_CPP_OPTS += -DLIBRARY=\"HS$(PACKAGE)\"
PACKAGE_CPP_OPTS += -DLIBDIR=\"$(libdir)\"

# Let the package configuration file refer to $(libdir) as
# ${pkglibdir}.  Note we can't use ${libdir} because ghc-pkg already
# redefines it to point to GHC's libdir (bug or feature?).
#
install :: package.conf.in
	$(CPP) $(RAWCPP_FLAGS) $(PKGCONF_CPP_EXTRA_OPTS) -DINSTALLING -x c $(PACKAGE_CPP_OPTS) $< \
	| sed -e 's/^#.*$$//g' -e 's/""//g' -e 's/, ]/ ]/g' \
	| $(GHC_PKG) --force --update-package

endif # $(STANDALONE_PACKAGE)

endif # $(way) == ""

# -----------------------------------------------------------------------------
# Building the static library libHS<pkg>.a

ifeq "$(STANDALONE_PACKAGE)" "NO"
HC = $(GHC_INPLACE)
endif

SRC_HSC2HS_OPTS += -I.

ifeq "$(NON_HS_PACKAGE)" ""
SRC_HC_OPTS 	+= -package-name $(PACKAGE)
SRC_HC_OPTS 	+= $(GhcLibHcOpts)
SRC_HC_OPTS     += $(patsubst %, -package %, $(PACKAGE_DEPS))
endif

LIBRARY      	= libHS$(PACKAGE)$(_way).a

WAYS         	= $(GhcLibWays)

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

endif # $(PACKAGE) /= ""

# install library (could be implicitly specified or explicitly, like libHS*_cbits.a)
INSTALL_LIBS  += $(LIBRARY)

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

GHCI_LIBRARY = $(patsubst lib%.a,%.o,$(LIBRARY))

INSTALL_LIBS += $(GHCI_LIBRARY)
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

# -----------------------------------------------------------------------------
# Doc building with Haddock

ifneq "$(NO_HADDOCK_DOCS)" "YES"

HS_PPS = $(addsuffix .raw-hs, $(basename $(filter-out $(EXCLUDED_HADDOCK_SRCS), $(HS_SRCS))))

HTML_DIR = html
HTML_DOC = $(HTML_DIR)/haddock.css

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
	$(GHC) $(HC_OPTS) -D__HADDOCK__ -E -cpp $< -o $<.tmp && sed -e 's/^#.*//' <$<.tmp >$@

%.raw-hs : %.hs
	$(GHC) $(HC_OPTS) -D__HADDOCK__ -E -cpp $< -o $<.tmp && sed -e 's/^#.*//' <$<.tmp >$@

install-docs :: $(HTML_DOC)
	@$(INSTALL_DIR) $(datadir)/html/libraries/$(PACKAGE)
	@for i in $(HTML_DIR)/*; do \
	   echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir)/html/libraries/$(PACKAGE); \
	   $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir)/html/libraries/$(PACKAGE); \
	done
	$(INSTALL_DATA) $(INSTALL_OPTS) $(PACKAGE).haddock $(datadir)/html/libraries/$(PACKAGE)

endif # HS_PPS
endif # NO_HADDOCK_DOCS

# -----------------------------------------------------------------------------

endif # $(LIBRARY) /= ""
