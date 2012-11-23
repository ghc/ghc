
-include mk/are-validating.mk

ifeq "$(Validating)" "YES"
include mk/validate-settings.mk
-include mk/validate.mk
else
-include $(firstword $(wildcard mk/$(GlobalCrossCompilePrefix)build.mk) mk/build.mk)
endif

ifeq "$(BINDIST)" "YES"
-include bindist.mk
endif

