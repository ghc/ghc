TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -d $1 ] || echo $1 missing
checkNotExists = [ ! -d $1  ] || echo $1 not missing

mhu-hidir:
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitP1 -v0
	$(call checkNotExists,dist)
	$(call checkExists, p1/dist)

