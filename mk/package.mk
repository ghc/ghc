# -----------------------------------------------------------------------------
# $Id: package.mk,v 1.6 2002/02/17 13:46:39 panne Exp $

ifneq "$(PACKAGE)" ""

# -----------------------------------------------------------------------------
# Build the package configuration file and tell the compiler about it.

ifeq "$(way)" ""

$(PACKAGE).conf.inplace   : $(PACKAGE).conf.in
	$(CPP) $(RAWCPP_FLAGS) -I$(GHC_INCLUDE_DIR) -x c $(PACKAGE_CPP_OPTS) $< \
		| sed 's/^#.*$$//g' >$@

$(PACKAGE).conf.installed : $(PACKAGE).conf.in
	$(CPP) $(RAWCPP_FLAGS) -I$(GHC_INCLUDE_DIR) -DINSTALLED -x c $(PACKAGE_CPP_OPTS) $< \
		| sed 's/^#.*$$//g' >$@

boot all :: $(PACKAGE).conf.inplace $(PACKAGE).conf.installed
	$(GHC_PKG_INPLACE) --update-package <$(PACKAGE).conf.inplace
	$(GHC_PKG_INPLACE)  -f $(GHC_DRIVER_DIR)/package.conf --update-package <$(PACKAGE).conf.installed

CLEAN_FILES += $(PACKAGE).conf.installed $(PACKAGE).conf.inplace

endif # $(way) == ""

# -----------------------------------------------------------------------------
# Building the static library libHS<pkg>.a

HC 	     	= $(GHC_INPLACE)

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

INSTALL_LIBS  += $(LIBRARY)

ifeq "$(DLLized)" "YES"
INSTALL_PROGS += $(DLL_NAME)
INSTALL_LIBS += $(patsubst %.a,%_imp.a, $(LIBRARY))
endif

# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
override datadir:=$(libdir)/imports/$(PACKAGE)

# If the lib consists of a hierachy of modules, we must retain the directory
# structure when we install the interfaces.
ifeq "$(HIERARCHICAL_LIB)" "YES"
INSTALL_DATAS_WITH_DIRS += $(HS_IFACES)
ifneq "$(ALL_DIRS)" ""
install ::
	@for i in $(ALL_DIRS); do \
		$(INSTALL_DIR) $(datadir)/$$i; \
	done
endif
else
INSTALL_DATAS += $(HS_IFACES)
endif

# -----------------------------------------------------------------------------
# Dependencies

MKDEPENDHS = $(GHC_INPLACE)
SRC_MKDEPENDC_OPTS += $(patsubst %,-I%,$(ALL_DIRS)) -I$(GHC_INCLUDE_DIR)

endif # $(PACKAGE) /= ""

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
	$(LD) -r $(LD_X) -o $@ $(LIBOBJS)

endif # DONT_WANT_STD_GHCI_LIB_RULE
endif # GhcWithInterpreter
endif # way

endif # $(LIBRARY) /= ""
