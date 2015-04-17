TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T9579_stackoverflow_rtsnone:
	'$(TEST_HC)' $(TEST_HC_OPTS) -rtsopts=none -fforce-recomp -with-rtsopts -K1m \
		 StackOverflow.hs -o T9579_stackoverflow_rtsnone

T9579_stackoverflow_rtssome:
	'$(TEST_HC)' $(TEST_HC_OPTS) -rtsopts=some -fforce-recomp -with-rtsopts -K1m \
		 StackOverflow.hs -o T9579_stackoverflow_rtssome

T9579_stackoverflow_rtsall:
	'$(TEST_HC)' $(TEST_HC_OPTS) -rtsopts=all  -fforce-recomp -with-rtsopts -K1m \
		 StackOverflow.hs -o T9579_stackoverflow_rtsall

T9579_outofheap_rtsnone:
	'$(TEST_HC)' $(TEST_HC_OPTS) -rtsopts=none -fforce-recomp -with-rtsopts -M1m \
		 OutOfHeap.hs -o T9579_outofheap_rtsnone

T9579_outofheap_rtssome:
	'$(TEST_HC)' $(TEST_HC_OPTS) -rtsopts=some -fforce-recomp -with-rtsopts -M1m \
		 OutOfHeap.hs -o T9579_outofheap_rtssome

T9579_outofheap_rtsall:
	'$(TEST_HC)' $(TEST_HC_OPTS) -rtsopts=all  -fforce-recomp -with-rtsopts -M1m \
		 OutOfHeap.hs -o T9579_outofheap_rtsall
