TARGET := sieve-vanilla.bin sieve-lwc.bin sieve-TMVar.bin sieve-vanilla-TMVar.bin

include ../../config.mk

TOP := ../../../
GHC_OPTS_EXTRA=-O2 -threaded -XBangPatterns -XCPP -XGeneralizedNewtypeDeriving -funbox-strict-fields -optc-O3 -debug

PROFILE_FLAGS := -DPROFILE_ENABLED -prof -auto-all -fprof-auto

#Uncomment the following line to enable profiled compilation
#GHC_OPTS_EXTRA += $(PROFILE_FLAGS)

all: $(TARGET)
