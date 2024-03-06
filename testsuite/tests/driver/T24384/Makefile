TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24384:
	'$(TEST_HC)' -M D.hs C.hs B.hs A.hs -dep-json dep.json -opt-json opt.json
	cat dep.json
	echo ''
	cat opt.json
