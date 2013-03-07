TARGET := sieve-vanilla.bin sieve-lwc.bin

include ../../config.mk

TOP := ../../../
GHC_OPTS_EXTRA=-XBangPatterns -O0 -threaded -prof -fprof-auto -auto-all

all: $(TARGET)
