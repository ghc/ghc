TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: test068
test068 :
	@echo Compiling
	$(TEST_HC) -v0 -fforce-recomp --make conc068.hs -o conc068 -threaded
	@echo Running
	yes abqszzzq 2>/dev/null | head -c 100000 | ./conc068 +RTS -N2
	@echo Done
