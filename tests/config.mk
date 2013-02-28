PROFILE := false
DEBUG := false
TOP := ..

ifeq ($(DEBUG),true)
  DEBUG_FLG := -debug
else
  DEBUG_FLG :=
endif


ifeq ($(PROFILE),true)
  PROFILE_FLG := -prof -auto-all
else
  PROFILE_FLG :=
endif

GHC_OPTS = -rtsopts --make

GHC := inplace/bin/ghc-stage1 $(DEBUG_FLG) $(PROFILE_FLG) $(GHC_OPTS)

all: $(TARGETS)

%.bin:	%.hs
	$(TOP)/$(GHC) $(GHC_OPTS_EXTRA) $< -o $@


%.cmm:  %.hs
	$(GHC) -c $< -ddump-cmm >$@

clean:
	rm -f *.bin *.o *.hi *.cmm *~ *.stat *.prof
