TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp -DSTRATEGIES -DTRANSCL_NESTED -package random -package parallel -package containers

# XXX: only speeds up without optimisation.  This is bad.  Could be
# due to the nfib delay mucking up load-balancing.
SRC_HC_OPTS += -O0

# <ver> <buffer> <unused> <dummy> <delay>
# ver = 4 (always)
# buffer = parBuffer size. 
#   10 is about optimal for 7.1, greater degrades perf (less so for local-gc)
# dummy = 999 (always)
# delay = larger for 
PROG_ARGS = 4 10 10 999 24

include $(TOP)/mk/target.mk

