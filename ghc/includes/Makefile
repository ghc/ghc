# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.11 1998/12/02 13:21:15 simonm Exp $
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

#
# All header files
#
H_FILES = $(wildcard *.h)

#
# Header file built from the configure script's findings
#
H_CONFIG = config.h

all :: $(H_CONFIG) NativeDefs.h

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

# ---------------------------------------------------------------------------
# Make NativeDefs.h for the NCG

C_PROG = mkNativeHdr
C_SRCS = mkNativeHdr.c

NativeDefs.h : mkNativeHdr
	./mkNativeHdr >NativeDefs.h

CLEAN_FILES += NativeDefs.h

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
override datadir:=$(libdir)/includes
INSTALL_DATAS += $(H_FILES) $(H_CONFIG)

#
# `make clean' settings:
#
CLEAN_FILES += $(H_CONFIG)

#
# Finally, slurp in the standard targets.
#
include $(TOP)/mk/target.mk
