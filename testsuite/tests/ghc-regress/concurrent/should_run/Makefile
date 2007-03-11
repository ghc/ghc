TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: test068
test068 :
	$(TEST_HC) -v0 -fforce-recomp --make conc068.hs -o conc068 -threaded
	yes abqszzzq 2>/dev/null | head -c 100000 | ./conc068 +RTS -N2
