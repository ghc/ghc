
-include mk/are-validating.mk

ifeq "$(Validating)" "YES"
include mk/validate-settings.mk
-include mk/validate.mk
else
-include mk/build.mk
endif
