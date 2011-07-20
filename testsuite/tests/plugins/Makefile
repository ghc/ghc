TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: plugins01 clean

plugins01:
	# This test is extremely fragile because any change in the output of the following
	# commands invalidates the output of the run.  In fact, we really want to ignore the
	# output resulting from building the simple-plugin, and only look at a few lines of
	# the output of the call to the TEST_HC (probably just the last 2).
	#
	# Suggestions to make this better gratefully recieved.
	(cd simple-plugin; make package)
	@$(RM) plugins01.hi plugins01.o
	"$(TEST_HC)" $(HC_OPTS) --make -v0 plugins01.hs -package-conf simple-plugin/local.package.conf -fplugin Simple.Plugin -fplugin-opt Simple.Plugin:Irrelevant_Option -package simple-plugin
	./plugins01

clean:
	cd simple-plugin && make clean
