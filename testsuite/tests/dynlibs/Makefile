TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T3807
T3807:
	$(RM) T3807Export_stub.c T3807Export_stub.h T3807Export_stub.o
	$(RM) T3807Export.o T3807Export.hi
	$(RM) T3807-export.o T3807-load.o
	$(RM) T3807test.so
	$(RM) T3807-load

	# GHC does not automatically link with the RTS when building shared
	# libraries. This is done to allow the RTS flavour to be chosen later (i.e.
	# when linking an executable).
	# Hence we must explicitly linking with the RTS here.
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 --make -dynamic -fPIC -shared T3807Export.hs T3807-export.c -o T3807test.so -flink-rts
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -no-auto-link-packages -no-hs-main T3807-load.c -o T3807-load -ldl
	./T3807-load

.PHONY: T4464dyn
T4464dyn:

.PHONY: T4464
T4464:
	$(RM) T4464B.o T4464C.o T4464H.hi T4464H.o
	$(RM) T4464H_stub.c T4464H_stub.h T4464H_stub.o
	$(RM) HS4464.dll HS4464.dll.a t4464.exe
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -shared T4464H.hs T4464B.c -optc-DRTSOPTS=RtsOptsSafeOnly -o HS4464.dll
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 T4464C.c HS4464.dll.a -o t4464.exe -no-hs-main
	-./t4464.exe
	echo "====="
	echo "=====" >&2
	$(RM) T4464B.o T4464C.o T4464H.hi T4464H.o
	$(RM) T4464H_stub.c T4464H_stub.h T4464H_stub.o
	$(RM) HS4464.dll HS4464.dll.a t4464.exe
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -shared T4464H.hs T4464B.c  -optc-DRTSOPTS=RtsOptsAll -o HS4464.dll
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 T4464C.c HS4464.dll.a -o t4464.exe -no-hs-main
	./t4464.exe
	echo "====="
	echo "=====" >&2
	$(RM) T4464B.o T4464C.o T4464H.hi T4464H.o
	$(RM) T4464H_stub.c T4464H_stub.h T4464H_stub.o
	$(RM) HS4464.dll HS4464.dll.a t4464.exe
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -dynamic -shared T4464H.hs T4464B.c  -optc-DRTSOPTS=RtsOptsAll -o HS4464.dll
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -dynamic T4464C.c -o t4464.exe -no-hs-main -L. -lHS4464
	./t4464.exe

.PHONY: T5373
T5373:
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -dynamic --make T5373A
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -dynamic --make T5373B -rtsopts
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -dynamic --make T5373C -rtsopts=some
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -dynamic --make T5373D -rtsopts=none
	-./T5373A +RTS -c 2>&1 | grep disabled
	-./T5373B +RTS -c 2>&1 | grep disabled
	-./T5373C +RTS -c 2>&1 | grep disabled
	-./T5373D +RTS -c 2>&1 | grep disabled

.PHONY: T13702
T13702:
	'$(TEST_HC)' -v0 -dynamic -rdynamic -fPIC -pie T13702.hs
	./T13702

.PHONY: T18072
T18072:
	$(RM) -rf T18072/
	mkdir T18072
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -outputdir T18072 \
      -dynamic -fPIC -c T18072.hs
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -outputdir T18072 \
      -dynamic -shared -flink-rts T18072/T18072.o -o T18072/T18072.so
	ldd T18072/T18072.so | grep libHSrts >/dev/null

.PHONY: T18072debug
T18072debug:
	$(RM) -rf T18072debug/
	mkdir T18072debug
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -outputdir T18072debug \
      -dynamic -fPIC -c T18072.hs
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -outputdir T18072debug \
      -dynamic -shared -flink-rts -debug T18072debug/T18072.o -o T18072debug/T18072.so
	ldd T18072debug/T18072.so | grep libHSrts | grep _debug >/dev/null

.PHONY: T18072static
T18072static:
	$(RM) -rf T18072static/
	mkdir T18072static
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -outputdir T18072static \
      -c T18072.hs
	'$(TEST_HC)' $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 -outputdir T18072static \
      -staticlib -fno-link-rts T18072static/T18072.o -o T18072static/T18072.a
	ar t T18072static/T18072.a | grep RtsSymbols.o > /dev/null && exit 1 || exit 0
