TARGET := chameneos-redux-vanilla.bin chameneos-redux-lwc.bin

include ../../config.mk

TOP := ../../../
GHC_OPTS_EXTRA=-O2 -threaded -XBangPatterns -XCPP -XGeneralizedNewtypeDeriving -funbox-strict-fields

all: $(TARGET)
