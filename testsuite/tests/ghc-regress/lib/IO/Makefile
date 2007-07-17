TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

test.concio001:
	@$(TEST_HC) --make -fforce-recomp -v0 concio001 -o concio001 
	@(sleep 1; echo x) | ./concio001

test.concio001.thr:
	@$(TEST_HC) --make -fforce-recomp -v0 -threaded concio001 -o concio001 
	@(sleep 1; echo x) | ./concio001
