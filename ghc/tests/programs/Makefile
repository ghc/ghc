TOP = .
include $(TOP)/mk/boilerplate.mk

NOT_THESE = CVS mk Makefile

NOT_THESE += hill_stk_oflow
#	Correctly fails to terminate

NOT_THESE += ipoole_spec_class
#	Dialogue style IO

NOT_THESE += areid_pass
#	Old-style I/O

NOT_THESE += north_lias
#	Deliberately causes divide by zero, and
#	we can't catch that yet

SUBDIRS = $(filter-out $(NOT_THESE), $(wildcard *))

include $(TOP)/mk/target.mk

