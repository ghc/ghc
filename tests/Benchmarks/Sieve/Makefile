TARGET := sieve-vanilla.bin sieve-lwc.bin sieve-TMVar.bin sieve-vanilla-TMVar.bin

include ../../config.mk

TOP := ../../../
GHC_OPTS_EXTRA=-threaded -XBangPatterns -XCPP -XGeneralizedNewtypeDeriving -funbox-strict-fields -O2 -optc-O3

PROFILE_FLAGS := -DPROFILE_ENABLED -prof -auto-all -fprof-auto

#Uncomment the following line to enable profiled compilation
#GHC_OPTS_EXTRA += $(PROFILE_FLAGS)

all: $(TARGET)
