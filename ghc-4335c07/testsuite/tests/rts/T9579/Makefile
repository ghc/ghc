TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T9579_stackoverflow_rtsnone:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=none -fforce-recomp -with-rtsopts -K1m \
		-outputdir tmp_T9579_stackoverflow_rtsnone \
		StackOverflow.hs -o T9579_stackoverflow_rtsnone

T9579_stackoverflow_rtssome:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=some -fforce-recomp -with-rtsopts -K1m \
		-outputdir tmp_T9579_stackoverflow_rtssome \
		StackOverflow.hs -o T9579_stackoverflow_rtssome

T9579_stackoverflow_rtsall:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=all  -fforce-recomp -with-rtsopts -K1m \
		-outputdir tmp_T9579_stackoverflow_rtsall \
		StackOverflow.hs -o T9579_stackoverflow_rtsall

T9579_stackoverflow_rtsall_no_suggestions:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=all -fforce-recomp -with-rtsopts -K1m \
		-no-rtsopts-suggestions \
		-outputdir tmp_T9579_stackoverflow_rtsall_no_suggestions \
		StackOverflow.hs -o T9579_stackoverflow_rtsall_no_suggestions

T9579_outofheap_rtsnone:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=none -fforce-recomp -with-rtsopts -M1m \
		-outputdir tmp_T9579_outofheap_rtsnone \
		OutOfHeap.hs -o T9579_outofheap_rtsnone

T9579_outofheap_rtssome:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=some -fforce-recomp -with-rtsopts -M1m \
		-outputdir tmp_T9579_outofheap_rtssome \
		OutOfHeap.hs -o T9579_outofheap_rtssome

T9579_outofheap_rtsall:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=all -fforce-recomp -with-rtsopts -M1m \
		-outputdir tmp_T9579_outofheap_rtsall \
		OutOfHeap.hs -o T9579_outofheap_rtsall

T9579_outofheap_rtsall_no_suggestions:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts=all -fforce-recomp -with-rtsopts -M1m \
		-no-rtsopts-suggestions \
		-outputdir tmp_T9579_outofheap_rtsall_no_suggestions \
		OutOfHeap.hs -o T9579_outofheap_rtsall_no_suggestions
