TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS= circsim constraints fibheaps fulsom gc_bench happy hash lcss \
    mutstore1 mutstore2 power spellcheck

# Discount treejoin: runs too quickly to see any speedup beyond a few procs
# Discount linear: heap is too small
# Discount cacheprof: heap is too small, runs too quickly, too much variation in the results
OTHER_SUBDIRS = treejoin linear cacheprof

date=$(shell date +%F)
# set 'tag' from the cmdline to add an extra tag to each logfile
logid=$(date)$(tag)

.DUMMY: allways threaded-1 threaded-2

HEAD ::
	$(MAKE) clean boot
	$(MAKE) -k EXTRA_HC_OPTS=-L$$HOME/builds/ghc/rts 2>&1 | tee log-$(logid)-HEAD

allways ::
	$(MAKE) clean boot
	$(MAKE) baseline
	$(MAKE) parways
	$(MAKE) analyse

parways ::
	$(MAKE) clean boot
	$(MAKE) threaded THREADS=1
	$(MAKE) threaded THREADS=2
	$(MAKE) threaded THREADS=3
	$(MAKE) threaded THREADS=4
	$(MAKE) threaded THREADS=5
	$(MAKE) threaded THREADS=6
	$(MAKE) threaded THREADS=7
	$(MAKE) threaded THREADS=8

# HEAP=OLD
HEAP=LARGE

# default
THREADS=1

# baseline with the old heap settings (-H10m)
baseline ::
	$(MAKE) -k HEAP=$(HEAP) 2>&1 | tee log-$(logid)-baseline

threaded ::
	$(MAKE) -k HEAP=$(HEAP) EXTRA_HC_OPTS=-threaded EXTRA_RUNTEST_OPTS="+RTS -qg1 -N$(THREADS) -RTS" 2>&1 | tee log-$(logid)-threaded-$(THREADS)

analyse ::
	../../utils/nofib-analyse/nofib-analyse log-$(logid)-baseline log-$(logid)-threaded-* >nofib-analyse-$(logid)

include $(TOP)/mk/target.mk

