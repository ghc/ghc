TOP = .
include $(TOP)/mk/boilerplate.mk

NOT_THESE = CVS mk Makefile \

NOT_THESE += hill_stk_oflow
#	Correctly fails to terminate

NOT_THESE += ipoole_spec_class
#	Dialogue style IO

#	areid_pass

SUBDIRS = $(filter-out $(NOT_THESE), $(wildcard *))

include $(TOP)/mk/target.mk

