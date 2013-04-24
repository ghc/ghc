TARGET := chameneos-redux-vanilla.bin chameneos-redux-lwc.bin

include ../../config.mk

TOP := ../../../
GHC_OPTS_EXTRA=-O2 -threaded -XBangPatterns -XCPP -XGeneralizedNewtypeDeriving -funbox-strict-fields -optc-O3

PROFILE_FLAGS := -DPROFILE_ENABLED -prof -auto-all -fprof-auto

#Uncomment the following line to enable profiled compilation
#GHC_OPTS_EXTRA += $(PROFILE_FLAGS)

all: $(TARGET)
