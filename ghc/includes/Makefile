TOP = ../..
UnlitSuffixRules = YES
include $(TOP)/ghc/mk/ghc.mk

# Literate header files
HLIT = $(patsubst %.lh, %.h, $(wildcard *.lh))

# Header files built by configure
HCONFIG = config.h platform.h

# Everything else
HFILES = stgdefs.h rtsdefs.h StgDirections.h StgMachDeps.h error.h \
  ieee-flpt.h gmp.h LLC.h HLC.h

ifeq ($(GhcWithNativeCodeGen),YES)
  ALLFILES = $(HLIT) $(HFILES) $(TARGETPLATFORM).h
else
  ALLFILES = $(HLIT) $(HFILES)
endif

$(TARGETPLATFORM).h : mkNativeHdr
	$(RM) $@
	./mkNativeHdr > $@ || ( rm $@ && exit 1 )

mkNativeHdr : mkNativeHdr.c $(HLIT) $(HFILES)
	$(GHC) -c mkNativeHdr.c
	$(CC) $(CFLAGS) -o mkNativeHdr mkNativeHdr.c

all :: $(ALLFILES)

install :: $(ALLFILES) $(HCONFIG)
	$(INSTALL) $(INSTDATAFLAGS) $(ALLFILES) $(INSTDATADIR_GHC)/includes
	$(INSTALL) $(INSTDATAFLAGS) $(HCONFIG) $(INSTLIBDIR_GHC)/includes

ifeq ($(GhcWithNativeCodeGen),YES)
  clean ::
	$(RM) $(HLIT) mkNativeHdr mkNativeHdr.c $(TARGETPLATFORM).h
else
  clean ::
	$(RM) $(HLIT) mkNativeHdr mkNativeHdr.c
endif
