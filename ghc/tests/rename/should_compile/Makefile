TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

SRC_HC_OPTS += -dcore-lint
# There used to be a -noC flag, but I removed it because
# Rn037Help needs to generate an interface file, and
# if -noC is on I can't see how to.  (Driver grottiness.)

# rn017_HC_OPTS = -hi -hi-with-exports
# rn025_HC_OPTS = -hi -hi-with-exports
# Rn037Help_HC_OPTS = -hi

CLEAN_FILES += Confused.hi Foo.hi Imp10.hi Imp100.hi Imp1000.hi \
	Imp500.hi Main.hi Mod10.hi OK.hi ShouldSucceed.hi Silly.hi Test.hi

include $(TOP)/mk/target.mk


