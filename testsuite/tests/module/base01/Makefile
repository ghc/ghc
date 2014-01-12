TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f GHC/*.o
	rm -f GHC/*.hi

base01:
	rm -f GHC/*.o
	rm -f GHC/*.hi
	'$(TEST_HC)' $(filter-out -fforce-recomp,$(TEST_HC_OPTS)) -XNoImplicitPrelude -package-name base -c GHC/Base.hs
	'$(TEST_HC)' $(filter-out -fforce-recomp,$(TEST_HC_OPTS)) -XNoImplicitPrelude -package-name base --make GHC.Foo

