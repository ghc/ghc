# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.22 2003/09/04 09:56:16 simonmar Exp $
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

#
# All header files
#
H_FILES = $(filter-out gmp.h,$(wildcard *.h)) gmp.h

#
# Options -- if we're building unregisterised, add a couple of -D's
#
ifeq "$(GhcUnregisterised)" "YES"
SRC_CC_OPTS += -DNO_REGS -DUSE_MINIINTERPRETER
endif

#
# Header file built from the configure script's findings
#
H_CONFIG = config.h

boot :: gmp.h

all :: $(H_CONFIG) NativeDefs.h

# gmp.h is copied from the GMP directory
gmp.h : $(FPTOOLS_TOP)/ghc/rts/gmp/gmp.h
	$(CP) $< $@

# The fptools configure script creates the configuration header file 
# and puts it in fptools/mk/config.h. We copy it down to here, prepending
# some make variables specifying cpp platform variables.
#
$(H_CONFIG) : $(FPTOOLS_TOP)/mk/config.h $(FPTOOLS_TOP)/mk/config.mk

$(H_CONFIG) :
	@echo "Creating $@..."
	@$(RM) $@
	@echo "#ifndef __FPTOOLS_CONFIG_H__"  >$@
	@echo "#define __FPTOOLS_CONFIG_H__" >>$@
	@echo >> $@
	@echo "#define HostPlatform_TYPE   $(HostPlatform_CPP)" >> $@
	@echo "#define TargetPlatform_TYPE $(TargetPlatform_CPP)" >> $@
	@echo "#define BuildPlatform_TYPE  $(BuildPlatform_CPP)" >> $@
	@echo >> $@
	@echo "#define $(HostPlatform_CPP)_HOST		1" >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET	1" >> $@
	@echo "#define $(BuildPlatform_CPP)_BUILD  	1" >> $@
	@echo >> $@
	@echo "#define $(HostArch_CPP)_HOST_ARCH	1" >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH	1" >> $@
	@echo "#define $(BuildArch_CPP)_BUILD_ARCH  	1" >> $@
	@echo "#define HOST_ARCH			\"$(HostArch_CPP)\"" >> $@
	@echo "#define TARGET_ARCH			\"$(TargetArch_CPP)\"" >> $@
	@echo "#define BUILD_ARCH			\"$(BuildArch_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(HostOS_CPP)_HOST_OS		1" >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS	1" >> $@  
	@echo "#define $(BuildOS_CPP)_BUILD_OS 		1" >> $@
	@echo "#define HOST_OS 				\"$(HostOS_CPP)\"" >> $@
	@echo "#define TARGET_OS			\"$(TargetOS_CPP)\"" >> $@
	@echo "#define BUILD_OS				\"$(BuildOS_CPP)\"" >> $@
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS		 " >> $@  
	@echo "#define $(IRIX_MAJOR)_TARGET_OS		1" >> $@  
	@echo "#endif					 " >> $@  
endif
	@echo >> $@
	@echo "#define $(HostVendor_CPP)_HOST_VENDOR	1" >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1" >> $@
	@echo "#define $(BuildVendor_CPP)_BUILD_VENDOR 	1" >> $@
	@cat $(FPTOOLS_TOP)/mk/$@ >> $@
	@echo "#endif /* __FPTOOLS_CONFIG_H__ */"          >> $@
	@echo "Done."

# ---------------------------------------------------------------------------
# Make DerivedConstants.h for the compiler

all :: DerivedConstants.h

mkDerivedConstants.c : $(H_CONFIG)

mkDerivedConstantsHdr : mkDerivedConstants.o
	$(CC) -o $@ $(CC_OPTS) $(LD_OPTS) mkDerivedConstants.o

DerivedConstants.h : mkDerivedConstantsHdr
	./mkDerivedConstantsHdr >DerivedConstants.h

CLEAN_FILES += mkDerivedConstantsHdr$(exeext) DerivedConstants.h

# ---------------------------------------------------------------------------
# Make NativeDefs.h for the NCG

all :: NativeDefs.h

mkNativeHdr.o : DerivedConstants.h

mkNativeHdr : mkNativeHdr.o
	$(CC) -o $@ $(CC_OPTS) $(LD_OPTS) mkNativeHdr.o

NativeDefs.h : mkNativeHdr
	./mkNativeHdr >NativeDefs.h

CLEAN_FILES += mkNativeHdr$(exeext) NativeDefs.h

# ---------------------------------------------------------------------------
# boot setup:
#
# Need config.h to make dependencies in the runtime system source.
#
boot :: all

#
# Install all header files
#
# Hackily set the install destination here:
#
# Note: we keep per-platform copies of all the include files
# (ditto for interface files). This is not *really* needed, but
# it gives (perhaps) a cleaner binary dist structure..might change.
#
override datadir:=$(libdir)/include
INSTALL_DATAS += $(H_FILES) $(H_CONFIG)

#
# `make clean' settings:
#
CLEAN_FILES += $(H_CONFIG)

#
# Finally, slurp in the standard targets.
#
include $(TOP)/mk/target.mk
