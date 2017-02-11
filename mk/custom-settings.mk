
-include mk/are-validating.mk

ifeq "$(Validating)" "YES"
include mk/flavours/validate.mk
-include mk/validate.mk
else
-include $(firstword $(wildcard mk/$(TargetPlatformFull)-build.mk) mk/build.mk)
GhcStage1HcOpts = -O2
endif

ifeq "$(BINDIST)" "YES"
-include bindist.mk
endif

