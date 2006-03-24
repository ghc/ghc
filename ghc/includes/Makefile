# -----------------------------------------------------------------------------

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

SRC_CC_OPTS += -I. -I../rts

#
# Header file built from the configure script's findings
#
H_CONFIG   = ghcautoconf.h
H_PLATFORM = ghcplatform.h

boot :: gmp.h

all :: $(H_CONFIG) $(H_PLATFORM)

# gmp.h is copied from the GMP directory
gmp.h : $(FPTOOLS_TOP)/ghc/rts/gmp/gmp.h
	$(CP) $< $@

# The fptools configure script creates the configuration header file and puts it
# in fptools/mk/config.h. We copy it down to here (without any PACKAGE_FOO
# definitions to avoid clashes), prepending some make variables specifying cpp
# platform variables.

ifneq "$(TARGETPLATFORM)"  "$(HOSTPLATFORM)"

$(H_CONFIG) :
	@echo "*** Cross-compiling: please copy $(H_CONFIG) from the target system"
	@exit 1

else

$(H_CONFIG) : $(FPTOOLS_TOP)/mk/config.h $(FPTOOLS_TOP)/mk/config.mk

$(H_CONFIG) : Makefile
	@echo "#ifndef __GHCAUTOCONF_H__"  >$@
	@echo "#define __GHCAUTOCONF_H__" >>$@
#	Turn '#define PACKAGE_FOO "blah"' into '/* #undef PACKAGE_FOO */'.
	@sed 's,^\([	 ]*\)#[	 ]*define[	 ][	 ]*\(PACKAGE_[A-Z]*\)[	 ][ 	]*".*".*$$,\1/* #undef \2 */,' $(FPTOOLS_TOP)/mk/config.h >> $@
	@echo "#endif /* __GHCAUTOCONF_H__ */"          >> $@
	@echo "Done."

endif

$(H_PLATFORM) : Makefile
	@echo "Creating $@..."
	@$(RM) $@
	@echo "#ifndef __GHCPLATFORM_H__"  >$@
	@echo "#define __GHCPLATFORM_H__" >>$@
	@echo >> $@
	@echo "#define BuildPlatform_TYPE  $(HostPlatform_CPP)" >> $@
	@echo "#define HostPlatform_TYPE   $(TargetPlatform_CPP)" >> $@
	@echo >> $@
	@echo "#define $(HostPlatform_CPP)_BUILD  1" >> $@
	@echo "#define $(TargetPlatform_CPP)_HOST  1" >> $@
	@echo >> $@
	@echo "#define $(HostArch_CPP)_BUILD_ARCH  1" >> $@
	@echo "#define $(TargetArch_CPP)_HOST_ARCH  1" >> $@
	@echo "#define BUILD_ARCH  \"$(HostArch_CPP)\"" >> $@
	@echo "#define HOST_ARCH  \"$(TargetArch_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(HostOS_CPP)_BUILD_OS  1" >> $@
	@echo "#define $(TargetOS_CPP)_HOST_OS  1" >> $@
	@echo "#define BUILD_OS  \"$(HostOS_CPP)\"" >> $@
	@echo "#define HOST_OS  \"$(TargetOS_CPP)\"" >> $@
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_HOST_OS" >> $@  
	@echo "#define $(IRIX_MAJOR)_HOST_OS  1" >> $@  
	@echo "#endif" >> $@  
endif
	@echo >> $@
	@echo "#define $(HostVendor_CPP)_BUILD_VENDOR  1" >> $@
	@echo "#define $(TargetVendor_CPP)_HOST_VENDOR  1" >> $@
	@echo "#define BUILD_VENDOR  \"$(HostVendor_CPP)\"" >> $@
	@echo "#define HOST_VENDOR  \"$(TargetVendor_CPP)\"" >> $@
	@echo >> $@
	@echo "/* These TARGET macros are for backwards compatibily... DO NOT USE! */" >> $@
	@echo "#define TargetPlatform_TYPE $(TargetPlatform_CPP)" >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET  1" >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH  1" >> $@
	@echo "#define TARGET_ARCH  \"$(TargetArch_CPP)\"" >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS  1" >> $@  
	@echo "#define TARGET_OS  \"$(TargetOS_CPP)\"" >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1" >> $@
	@echo >> $@
	@echo "#endif /* __GHCPLATFORM_H__ */"          >> $@
	@echo "Done."

# ---------------------------------------------------------------------------
# Make DerivedConstants.h for the compiler

all :: DerivedConstants.h

ifneq "$(TARGETPLATFORM)" "$(HOSTPLATFORM)"

DerivedConstants.h :
	@echo "*** Cross-compiling: please copy DerivedConstants.h from the target system"
	@exit 1

else

mkDerivedConstants.c : $(H_CONFIG) $(H_PLATFORM)

mkDerivedConstantsHdr : mkDerivedConstants.o
	$(CC) -o $@ $(CC_OPTS) $(LD_OPTS) mkDerivedConstants.o

DerivedConstants.h : mkDerivedConstantsHdr
	./mkDerivedConstantsHdr >$@

endif

CLEAN_FILES += mkDerivedConstantsHdr$(exeext) DerivedConstants.h

# -----------------------------------------------------------------------------
#

all :: GHCConstants.h

ifneq "$(TARGETPLATFORM)" "$(HOSTPLATFORM)"

GHCConstants.h :
	@echo "*** Cross-compiling: please copy DerivedConstants.h from the target system"
	@exit 1

else

mkGHCConstants : mkGHCConstants.o
	$(CC) -o $@ $(CC_OPTS) $(LD_OPTS) mkGHCConstants.o

mkGHCConstants.o : mkDerivedConstants.c
	$(CC) -o $@ $(CC_OPTS) -c $<  -DGEN_HASKELL

GHCConstants.h : mkGHCConstants
	./mkGHCConstants >$@

endif

CLEAN_FILES += mkGHCConstants$(exeext) GHCConstants.h

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
INSTALL_DATAS += $(H_FILES) $(H_CONFIG) $(H_PLATFORM)

#
# `make clean' settings:
#
CLEAN_FILES += $(H_CONFIG) $(H_PLATFORM)

#
# Finally, slurp in the standard targets.
#
include $(TOP)/mk/target.mk
