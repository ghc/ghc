TOP = .
include $(TOP)/mk/boilerplate.mk

NOT_THESE = CVS mk Makefile make.log

NOT_THESE += hill_stk_oflow
#	Correctly fails to terminate

NOT_THESE += ipoole_spec_class
#	Dialogue style IO

NOT_THESE += areid_pass
#	Old-style I/O

NOT_THESE += north_lias
#	Deliberately causes divide by zero, and
#	we can't catch that yet

NOT_THESE += andy_cherry barton-mangler-bug callback cvh_unboxing dmgob_native1 dmgob_native2 fast2haskell fexport jtod_circint okeefe_neural
#	doesn't compile

NOT_THESE += jeff-bug lennart_array
#       compiles but doesn't run

SUBDIRS = $(filter-out $(NOT_THESE), $(wildcard *))

include $(TOP)/mk/target.mk

