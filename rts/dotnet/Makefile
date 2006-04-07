#
# .NET interop for GHC.
#
#  (c) 2003, sof.
# 
TOP=../..
include $(TOP)/mk/boilerplate.mk

all :: Invoker.dll Invoke.o

#
# To compile the dotnet interop bits, you need to have the
# .NET Framework SDK or VS.NET installed. The following
# apps are used:
# 
MCPP=cl
TLBEXP=tlbexp
REGASM=regasm
GACUTIL=gacutil

Invoker.dll : Invoker.obj 
	$(MCPP) /LD /clr /o Invoker.dll Invoker.obj
	$(TLBEXP) Invoker.dll
	$(REGASM) Invoker.dll
	$(GACUTIL) /i Invoker.dll

Invoker.obj : Invoker.cpp Invoker.h
	$(MCPP) /LD /clr /c Invoker.cpp

CLEAN_FILES += $(wildcard *.obj *.dll *.tlb)

# ToDo: 
#   - switch to /ir (i.e., copy it into the GAC.)
#   - sort out installation story.

# drop the assembly
remove : 
	$(GACUTIL) /u Invoker

#
# NOTE: For DotnetCc a version of gcc later than gcc-2.95 is
#       required (I'm using the gcc-3.2 snapshot that comes with mingw-2)
#
ifeq "$(DotnetCc)" ""
DotnetCc=$(CC)
endif
DotnetCcOpts=$(CC_OPTS) $(DOTNET_EXTRA_CC_OPTS)
SRC_CC_OPTS += -I$(TOP)/includes

Invoke.o : Invoke.c 
	$(DotnetCc) $(DotnetCcOpts) -c $< -o $@ 

include $(TOP)/mk/target.mk
