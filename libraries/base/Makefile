# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.33 2002/06/20 16:12:58 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ALL_DIRS = \
	Control \
	Control/Concurrent \
	Control/Monad \
	Control/Monad/ST \
	Data \
	Data/Array \
	Data/STRef \
	Database \
	Debug \
	Debug/QuickCheck \
	FileFormat \
	Foreign \
	Foreign/C \
	Foreign/Marshal \
	GHC \
	Hugs \
	Language \
	Network \
	NHC \
	System \
	System/Console \
	System/Mem \
	System/IO \
	Text \
	Text/Html \
	Text/PrettyPrint \
	Text/ParserCombinators \
	Text/ParserCombinators/Parsec \
	Text/Regex \
	Text/Show \
	Text/Read

PACKAGE = base

SRC_HC_OPTS += -fglasgow-exts -cpp -Iinclude
SRC_HSC2HS_OPTS += -Iinclude

# Make sure we can get hold of regex.h
ifneq "$(HavePosixRegex)" "YES"
SRC_HC_OPTS     += -Icbits/regex
SRC_HSC2HS_OPTS += -Icbits/regex
endif

# -----------------------------------------------------------------------------
# Per-module flags

# ESSENTIAL, for getting reasonable performance from the I/O library:
SRC_HC_OPTS += -funbox-strict-fields

# -----------------------------------------------------------------------------
# PrimOpWrappers

GHC/PrimopWrappers.hs: $(GHC_COMPILER_DIR)/prelude/primops.txt
	rm -f $@
	$(GHC_GENPRIMOP) --make-haskell-wrappers < $< > $@

boot :: GHC/PrimopWrappers.hs

EXTRA_SRCS  += GHC/PrimopWrappers.hs
CLEAN_FILES += GHC/PrimopWrappers.hs

#-----------------------------------------------------------------------------
# 	Building the library for GHCi
#
# The procedure differs from that in fptools/mk/target.mk in one way:
#  (*) on Win32 we must split it into two, because a single .o file can't
#      have more than 65536 relocations in it [due to a bug in the GNU
#      linker.]

OBJECT_FILEFORMAT=unknown
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
OBJECT_FILEFORMAT=PEi
endif
ifeq "$(TARGETPLATFORM)" "i386-unknown-cygwin32"
OBJECT_FILEFORMAT=PEi
endif

ifeq "$(OBJECT_FILEFORMAT)" "PEi"

# Turn off standard rule which creates HSbase.o from LIBOBJS.
DONT_WANT_STD_GHCI_LIB_RULE=YES

GHCI_LIBOBJS = $(HS_OBJS)

INSTALL_LIBS += HSbase1.o HSbase2.o HSbase3.o

endif # OBJECT_FILEFORMAT = PEi


# -----------------------------------------------------------------------------
# Doc building with Haddock

EXCLUDED_HADDOCK_SRCS = \
	Data/Generics.hs \
	GHC/PArr.hs

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

ifeq "$(OBJECT_FILEFORMAT)" "PEi"
HSbase.o : $(GHCI_LIBOBJS)
	$(LD) -r $(LD_X) -o HSbase1.o $(filter     GHC/%, $(GHCI_LIBOBJS))
	$(LD) -r $(LD_X) -o HSbase2.o $(filter     Text/%, $(GHCI_LIBOBJS))
	$(LD) -r $(LD_X) -o HSbase3.o $(filter-out GHC/% Text/%, $(GHCI_LIBOBJS))
	@touch HSbase.o
endif # OBJECT_FILEFORMAT = PEi

