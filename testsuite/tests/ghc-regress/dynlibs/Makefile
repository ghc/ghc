TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T3807
T3807:
	$(RM) T3807Export_stub.c T3807Export_stub.h T3807Export_stub.o
	$(RM) T3807Export.o T3807Export.hi
	$(RM) T3807-export.o T3807-load.o
	$(RM) T3807test.so
	$(RM) T3807-load
	'$(TEST_HC)' -v0 --make -dynamic -fPIC -shared T3807Export.hs T3807-export.c -o T3807test.so -lHSrts-ghc`'$(TEST_HC)' --numeric-version`
	'$(TEST_HC)' T3807-load.c -o T3807-load -ldl
	./T3807-load

