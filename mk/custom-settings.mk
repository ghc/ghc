
-include mk/are-validating.mk

ifeq "$(Validating)" "YES"
include mk/flavours/validate.mk
-include mk/validate.mk
else
# -include $(firstword $(wildcard mk/$(TargetPlatformFull)-build.mk) mk/build.mk)
GhcHcOpts  = 
GhcLibWays := $(filter v dyn,$(GhcLibWays))
GhcLibHcOpts += -O
GhcStage2HcOpts += -O
endif

ifeq "$(BINDIST)" "YES"
-include bindist.mk
endif

