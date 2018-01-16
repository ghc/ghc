TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = parfib partree sumeuler matmult ray gray prsa mandel queens \
          coins blackscholes minimax nbody transclos

OTHER_SUBDIRS = cfd dcbm linsolv nbody partak quicksort threadfib warshall

# CPP SYMBOLS
#
#  -DSTRATEGIES_2  to use the version 2 strategies library (default is 3)

# --------------
# Old benchmarks we need to either update or kill:
# 
# NESL: a collection of benchmarks based on NESL examples
# bom: bill of materials?
# minimax: looks like it was written for GRIP, needs a lot of updating
# par001: looks like a test, not a benchmark
# parfact: parallel factorial
# partak: program needs work to make it parallel
# soda, soda7: small word-search algorithm
# --------------

include $(TOP)/mk/target.mk
