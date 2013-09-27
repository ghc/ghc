TARGET := chameneos-redux-vanilla.bin chameneos-redux-lwc.bin chameneos-redux-TMVar.bin chameneos-redux-vanilla-TMVar.bin

include ../../config.mk

TOP := ../../../
EXTRA_LIBS=/scratch/chandras/install
GHC_OPTS_EXTRA=-XDeriveDataTypeable -XBangPatterns -XCPP -XGeneralizedNewtypeDeriving -funbox-strict-fields -ipqueue-1.2.1 -O2 -threaded

PROFILE_FLAGS := -DPROFILE_ENABLED -prof -fprof-auto -auto -auto-all -caf-all

#Uncomment the following line to enable profiled compilation
#GHC_OPTS_EXTRA += $(PROFILE_FLAGS)

all: $(TARGET)
