ifeq "$(NEWBUILD)" "NO"
TOP:=$(TOP)/..
include $(TOP)/mk/target.mk
TOP:=$(TESTSUITE_TOP)
endif
