TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# This makefile rule only exists because TEST_HC is set in boilerplate.mk
# and otherwise the TEST_HC variable wouldn't be set in the context of the bash script
T12983:
	TEST_HC="$(TEST_HC)" ./build.sh
