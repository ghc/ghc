TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ALL_DIRS = \
	Control \
	Control/Concurrent \
	Control/Parallel \
	Control/Monad \
	Control/Monad/ST \
	Data \
	Data/ByteString \
	Data/ByteString/Lazy \
	Data/Generics \
	Data/Array \
	Data/Array/IO \
	Data/STRef \
	Debug \
	Foreign \
	Foreign/C \
	Foreign/Marshal \
	GHC \
	System \
	System/Console \
	System/Mem \
	System/IO \
	System/Posix \
	System/Process \
	System/Directory \
	Text \
	Text/PrettyPrint \
	Text/ParserCombinators \
	Text/Show \
	Text/Read

PACKAGE = base
VERSION = 2.0

SRC_HC_OPTS += -fglasgow-exts -cpp -Iinclude -"\#include" HsBase.h
SRC_HSC2HS_OPTS += -Iinclude -I$(GHC_INCLUDE_DIR)

# -----------------------------------------------------------------------------
# Per-module flags

# ESSENTIAL, for getting reasonable performance from the I/O library:
SRC_HC_OPTS += -funbox-strict-fields

# -----------------------------------------------------------------------------
# PrimOpWrappers

# These two lines are required for pre-processing compiler/prelude/primops.txt
SRC_CPP_OPTS += -I$(GHC_INCLUDE_DIR)
SRC_CPP_OPTS += ${GhcCppOpts}

ifeq "$(BootingFromHc)" "YES"
GHC/PrimopWrappers.hs:
	touch GHC/PrimopWrappers.hs
else
GHC/PrimopWrappers.hs: $(GHC_COMPILER_DIR)/prelude/primops.txt GHC/Prim.hs
	@$(RM) $@
	$(GENPRIMOP) --make-haskell-wrappers < $< > $@
endif

GHC/Prim.hs: $(GHC_COMPILER_DIR)/prelude/primops.txt
	@$(RM) $@
	$(GENPRIMOP) --make-haskell-source < $< > $@

EXCLUDED_SRCS = GHC/Prim.hs
EXTRA_HADDOCK_SRCS = GHC/Prim.hs

boot :: GHC/PrimopWrappers.hs

EXTRA_SRCS  += GHC/PrimopWrappers.hs
CLEAN_FILES += GHC/PrimopWrappers.hs

# -----------------------------------------------------------------------------
ifneq "$(BootingFromHc)" "YES"
STUBOBJS += \
   Control/Concurrent_stub.$(way_)o

CLEAN_FILES += $(STUBOBJS) \
   Control/Concurrent_stub.[ch]
endif

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
#DONT_WANT_STD_GHCI_LIB_RULE=YES

GHCI_LIBOBJS = $(HS_OBJS)

INSTALL_LIBS += HSbase.o

endif # OBJECT_FILEFORMAT = PEi


# -----------------------------------------------------------------------------
# Doc building with Haddock

EXCLUDED_HADDOCK_SRCS = \
	GHC/PrimopWrappers.hs \
	GHC/PArr.hs

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)" \
	--no-implicit-prelude

# -----------------------------------------------------------------------------

GHC/ForeignPtr.o Data/Array/IO/Internals.o Data/Array/Base.o \
  Data/Generics/Instances.o Data/Complex.o Data/Array.o Data/STRef.o \
  Data/Dynamic.o Data/Typeable.o Data/PackedString.o System/Mem/Weak.o \
  System/Mem/StableName.o System/Posix/Types.o Control/Monad/ST.o \
  Control/Exception.o Foreign/C/Types.o Foreign/ForeignPtr.o: include/Typeable.h

System/Posix/Types.o Foreign/C/Types.o: include/CTypes.h

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += base.buildinfo config.cache config.status

include $(TOP)/mk/target.mk
