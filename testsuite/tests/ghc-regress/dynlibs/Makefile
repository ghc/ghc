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

.PHONY: T4464
T4464:
	$(RM) T4464B.o T4464C.o T4464H.hi T4464H.o
	$(RM) T4464H_stub.c T4464H_stub.h T4464H_stub.o
	$(RM) HS4464.dll HS4464.dll.a t4464.exe
	'$(TEST_HC)' -v0 -shared T4464H.hs T4464B.c -o HS4464.dll
	'$(TEST_HC)' -v0 T4464C.c HS4464.dll.a -o t4464.exe
	-./t4464.exe
	echo "====="
	echo "=====" >&2
	$(RM) T4464B.o T4464C.o T4464H.hi T4464H.o
	$(RM) T4464H_stub.c T4464H_stub.h T4464H_stub.o
	$(RM) HS4464.dll HS4464.dll.a t4464.exe
	'$(TEST_HC)' -v0 -shared T4464H.hs T4464B.c -o HS4464.dll -rtsopts
	'$(TEST_HC)' -v0 T4464C.c HS4464.dll.a -o t4464.exe
	./t4464.exe

