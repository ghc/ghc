TOP = .
include $(TOP)/mk/boilerplate.mk

SUBDIRS = timeout

CLEAN_FILES += mk/wordsize.mk

all ::
	@echo "To run the tests, go into tests/ghc-regress and say \`make'."
	@echo "More information about configuring and running the testsuite"
	@echo "can be found in the file \`README' in this directory."

boot ::
	$(CPP) $(RAWCPP_FLAGS) -x c mk/wordsize.mk.in > mk/wordsize.mk

include $(TOP)/mk/target.mk
