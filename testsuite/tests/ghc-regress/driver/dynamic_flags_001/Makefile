TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Dynamic flags tests

clean:
	rm -f *.o *.hi

# 001: Flags turned on in one module's OPTIONS_GHC pragma shouldn't
#      be on in when compiling other modules

dynamic_flags_001: clean
	$(TEST_HC) --make -Werror -v0 C.hs
