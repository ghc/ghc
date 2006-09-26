TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f GHC/*.o GHC/*.hi

base01:
	$(TEST_HC) -fno-implicit-prelude -package-name base -c GHC/Base.hs
	$(TEST_HC) -fno-implicit-prelude -package-name base --make GHC.Foo

