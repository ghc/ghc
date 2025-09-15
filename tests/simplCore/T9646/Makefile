TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS += -O2 -dsuppress-uniques -dsuppress-all -ddump-to-file -ddump-ds \
				-ddump-simpl -ddump-simpl-iterations -ddump-simpl-stats

clean :
	$(RM) -f *.o *.hi *.dump* cbits/primitive-memops.o T9646
