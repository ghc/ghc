TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T10672_x64:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=none -fforce-recomp -lgcc_s_seh-1 -package system-cxx-std-lib
		Main.hs Printf.hs cxxy.cpp
