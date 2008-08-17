
-include $(TOP)/mk/are-validating.mk

ifeq "$(Validating)" "YES"
include $(TOP)/mk/validate-settings.mk
-include $(TOP)/mk/validate.mk
else
-include $(TOP)/mk/build.mk
endif
