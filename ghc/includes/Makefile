#
# ghc/includes
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

#
# Just to make sure, no ways stuff in here, please.
#
override WAYS=

# De-litted header files
LH_FILES=$(wildcard *.lh)

DELIT_H_FILES = $(patsubst %.lh, %.h, $(LH_FILES))

#
# Header file built from the configure script's findings
#
H_CONFIG = config.h


# Everything else
H_FILES = stgdefs.h rtsdefs.h StgDirections.h StgMachDeps.h error.h \
  ieee-flpt.h gmp.h LLC.h HLC.h

MKDEPENDC_SRCS=mkNativeHdr.c

ALL_FILES = $(DELIT_H_FILES) $(H_FILES)
ifeq ($(GhcWithNativeCodeGen),YES)
ALL_FILES += $(TARGETPLATFORM).h
endif


#
# In main/Signals we need to distinguish between irix5 and irix6,
# so we suitably mangle HostOS_FULL to get at the major version.
# (A hack, for sure - ToDo: consider systematically adding more
# fine-grained OS info to this Makefile/configure soup )

ifeq "$(HostOS_CPP)" "irix"
IRIX_MAJOR = $(shell echo $(HostOS_Full) | sed 's/\(irix[^.]*\).*$$/\1/' )
endif

#
# The fptools configure script creates the configuration header file 
# and puts it in fptools/mk/config.h. We copy it down to here, prepending
# some make variables specifying cpp platform variables.
#
$(H_CONFIG) : $(FPTOOLS_TOP)/mk/config.h

$(H_CONFIG) :
	@echo "Creating $@..."
	@$(RM) $@
	@echo "#define HostPlatform_TYPE   $(HostPlatform_CPP)"  > $@
	@echo "#define TargetPlatform_TYPE $(HostPlatform_CPP)" >> $@
	@echo "#define BuildPlatform_TYPE  $(HostPlatform_CPP)" >> $@
	@echo >> $@
	@echo "#define $(HostPlatform_CPP)_HOST		1" >> $@
	@echo "#define $(HostPlatform_CPP)_TARGET	1" >> $@
	@echo "#define $(HostPlatform_CPP)_BUILD  	1" >> $@
	@echo >> $@
	@echo "#define $(HostArch_CPP)_HOST_ARCH	1" >> $@
	@echo "#define $(HostArch_CPP)_TARGET_ARCH	1" >> $@
	@echo "#define $(HostArch_CPP)_BUILD_ARCH  	1" >> $@
	@echo "#define HOST_ARCH			\"$(HostArch_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(HostOS_CPP)_HOST_OS		1" >> $@
	@echo "#define $(HostOS_CPP)_TARGET_OS		1" >> $@  
	@echo "#define $(HostOS_CPP)_BUILD_OS 		1" >> $@
	@echo "#define HOST_OS 				\"$(HostOS_CPP)\"" >> $@
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS		 " >> $@  
	@echo "#define $(IRIX_MAJOR)_TARGET_OS		1" >> $@  
	@echo "#endif					 " >> $@  
endif
	@echo >> $@
	@echo "#define $(HostVendor_CPP)_HOST_VENDOR	1" >> $@
	@echo "#define $(HostVendor_CPP)_TARGET_VENDOR  1" >> $@
	@echo "#define $(HostVendor_CPP)_BUILD_VENDOR 	1" >> $@
	@cat $(FPTOOLS_TOP)/mk/$@ >> $@
	@echo "Done."

$(TARGETPLATFORM).h : mkNativeHdr
	$(RM) $@
	./mkNativeHdr > $@ || ( rm $@ && exit 1 )

#
# Building mkNativeHdr using the Haskell compiler
# to do it (ghc really).
#
mkNativeHdr : $(HLIT) $(HFILES) mkNativeHdr.c
	$(CC) -o mkNativeHdr mkNativeHdr.c

all :: $(H_CONFIG) $(ALL_FILES)

#
# boot setup:
#
# When building the dependencies in runtime/ , lib/ we need to get
# at the de-litted versions of includes/, hence we arrange the
# `depend' target to depend on `all'.
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
override datadir:=$(libdir)/includes
INSTALL_DATAS += $(DELIT_H_FILES) $(H_FILES) $(TARGETPLATFORM).h $(H_CONFIG)

#
# `make clean' settings:
#
CLEAN_FILES += $(DELIT_H_FILES) $(H_CONFIG) mkNativeHdr.o mkNativeHdr

ifeq ($(GhcWithNativeCodeGen),YES)
CLEAN_FILES += $(TARGETPLATFORM).h
endif

#
# Finally, slurp in the standard targets.
#
include $(TOP)/mk/target.mk
