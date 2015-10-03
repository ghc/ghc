TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T10672_x64:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=none -fforce-recomp -lgcc_s_seh-1 -lstdc++-6 \
		Main.hs Printf.hs cxxy.cpp

T10672_x86:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=none -fforce-recomp -lgcc_s_dw2-1 -lstdc++-6 \
		Main.hs Printf.hs cxxy.cpp
