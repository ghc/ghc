TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# This makefile rule only exists because TEST_HC is set in boilerplate.mk
# and otherwise the TEST_HC variable wouldn't be set in the context of the bash script
rts-check-symbols:
	TOP="$(TOP)" TEST_HC_OPTS="$(TEST_HC_OPTS)" TEST_HC="$(TEST_HC)" ./rts-check-symbols.sh
