# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2001/07/04 10:48:16 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(way)" ""
SUBDIRS = cbits
else
SUBDIRS=
endif

ALL_DIRS = \
	Control \
	Control/Concurrent \
	Control/Monad \
	Control/Monad/ST \
	Data \
	Data/Array \
	Database \
	Debug \
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
	System/IO \
	Text \
	Text/Show

PRE_SRCS += $(wildcard $(patsubst %, %/*.hsc, $(ALL_DIRS)))
SRC_HSC2HS_OPTS += -Iinclude -I.

ALL_HS_SRCS = $(wildcard $(patsubst %, %/*.hs, . $(ALL_DIRS)))
ALL_LHS_SRCS += $(wildcard GHC/*.lhs)
ALL_HS_OBJS = $(patsubst %.hs, %.o, $(ALL_HS_SRCS)) \
	$(patsubst %.lhs, %.o, $(ALL_LHS_SRCS))
ALL_HS_HIS = $(patsubst %.o, %.hi, $(ALL_HS_OBJS))

srcs : $(HS_SRCS) GHC/Prim.$(way_)hi

# dependencies between .hsc files
GHC/IO.hs : GHC/Handle.hs

GHC/Prim.$(way_)hi : GHC/Prim.hi-boot
	cp $< $@

SRC_HC_OPTS += -cpp -fglasgow-exts -fvia-C -I$(FPTOOLS_TOP)/ghc/includes -Iinclude -package-name core -H128m $(GhcLibHcOpts)

LIBNAME = libHScore$(_way).a

CLEAN_FILES += $(ALL_HS_OBJS) $(ALL_HS_HIS)

all :: $(LIBNAME)

lib : srcs
	$(GHC_INPLACE) $(HC_OPTS) --make $(ALL_HS_SRCS) $(ALL_LHS_SRCS)

$(LIBNAME) : lib
	$(RM) $@
	$(AR) $(AR_OPTS) $@ $(ALL_HS_OBJS)
	$(RANLIB) $@

%.o : %.hs
	$(GHC_INPLACE) $(HC_OPTS) --make $<
%.o : %.lhs
	$(GHC_INPLACE) $(HC_OPTS) --make $<

include $(TOP)/mk/target.mk
