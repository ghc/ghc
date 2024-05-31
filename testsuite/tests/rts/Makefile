TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

outofmem-prep::
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make -fforce-recomp outofmem.hs -o outofmem

outofmem::
	$(MAKE) -s --no-print-directory outofmem-prep
	@ulimit -m 10000000 2>/dev/null; ./outofmem || echo "exit($$?)"

outofmem2-prep::
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts --make -fforce-recomp outofmem2.hs -o outofmem2

outofmem2::
	$(MAKE) -s --no-print-directory outofmem2-prep
	@ulimit -m 1000000 2>/dev/null; ./outofmem2 +RTS -M5m -RTS || echo "exit($$?)"

.PHONY: T4059
T4059:
	$(RM) T4059_c.o T4059.o T4059.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T4059 T4059_c.c
	./T4059

exec_signals-prep:
	$(CC) -o exec_signals_child exec_signals_child.c
	$(CC) -o exec_signals_prepare exec_signals_prepare.c

.PHONY: T4850
T4850:
	$(RM) T4850.o T4850.hi T4850$(exeext)
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -rtsopts -debug -threaded --make T4850
	./T4850 +RTS -s 2>&1 | grep TASKS | sed 's/^ *TASKS: *\([0-9]*\).*$$/\1/'

.PHONY: T5423
T5423:
	$(RM) T5423_cmm.o T5423.o T5423.hi T5423$(exeext)
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c T5423_cmm.cmm
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c T5423.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c T5423_c.c
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 T5423.o T5423_cmm.o T5423_c.o -o T5423$(exeext)
	./T5423

.PHONY: T9405
T9405:
	@'$(TEST_HC)' $(TEST_HC_OPTS) -ticky -rtsopts T9405.hs; \
		./T9405 +RTS -rT9405.ticky & \
		sleep 0.2; \
		kill -2 $$!; \
		wait $$!; \
		[ -e T9405.ticky ] || echo "Error: Ticky profile doesn't exist"; \
		[ -s T9405.ticky ] || echo "Error: Ticky profile is empty"; \
		echo Ticky-Ticky;

# Naming convention: 'T5423_' obj-way '_' obj-src
# obj-way ::= v | dyn
# obj-src ::= gcc // using __attribute__(constructor)
#           | asm // manually laid out sections
# $(0) = obj-src

T6006_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6006.hs

T8124_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8124.hs

.PHONY: T7037
T7037:
	$(RM) 7037.o 7037.hi 7037$(exeext)
	"$(TEST_HC)" $(TEST_HC_OPTS) T7037.hs -v0
	"$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS)) T7037_main.c -v0 -o T7037_main -no-hs-main
	./T7037_main

T7040_ghci_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghciWayFlags) -c T7040_ghci_c.c

.PHONY: T10296a
T10296a:
	$(RM) T10296a_c.o T10296a.o T10296a.hi T10296a_stub.h
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -threaded T10296a.hs T10296a_c.c -o T10296a
	./T10296a +RTS -N2

 .PHONY: T11788
T11788:
	"$(TEST_HC)" -c T11788.c -o T11788_obj.o
	"$(AR)" rsT libT11788.a T11788_obj.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) T11788.hs -lT11788 -L"$(PWD)"

 .PHONY: T12497
T12497:
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) T12497.hs

.PHONY: T13617
T13617:
	"$(TEST_CC)"  -O3 -ffast-math -ftree-vectorize -c T13617.c -o T13617_sse.o
	echo main | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T13617.hs T13617_sse.o

.PHONY: T14695
T14695:
	echo ":quit" | LD_LIBRARY_PATH="foo:" "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE))

.PHONY: InternalCounters
InternalCounters:
	"$(TEST_HC)" +RTS -s --internal-counters -RTS 2>&1 | grep "Internal Counters"
	-"$(TEST_HC)" +RTS -s -RTS 2>&1 | grep "Internal Counters"

.PHONY: KeepCafsFail
KeepCafsFail:
	"$(TEST_HC)" -c -g -v0 KeepCafsBase.hs KeepCafs1.hs KeepCafs2.hs
	"$(TEST_HC)" -g -v0 KeepCafsMain.hs KeepCafsBase.o -debug -rdynamic -fwhole-archive-hs-libs $(KEEPCAFS)
	./KeepCafsMain 2>&1 || echo "exit($$?)"

.PHONY: KeepCafs
KeepCafs:
	"${MAKE}" KeepCafsFail KEEPCAFS=-fkeep-cafs

.PHONY: EventlogOutput1
EventlogOutput1:
	"$(TEST_HC)" -v0 EventlogOutput.hs
	./EventlogOutput +RTS -l -olhello.eventlog
	ls hello.eventlog >/dev/null

.PHONY: EventlogOutput2
EventlogOutput2:
	"$(TEST_HC)" -v0 EventlogOutput.hs
	./EventlogOutput +RTS -l
	ls EventlogOutput.eventlog >/dev/null

.PHONY: EventlogOutputNull
EventlogOutputNull:
	"$(TEST_HC)" -rtsopts -v0 EventlogOutput.hs
	./EventlogOutput +RTS -l --null-eventlog-writer
	test ! -e EventlogOutput.eventlog

.PHONY: T20199
T20199:
	"$(TEST_HC)" -no-hs-main -optcxx-std=c++11 -v0 T20199.cpp -o T20199
	./T20199

.PHONY: EventlogOutput_IPE
EventlogOutput_IPE:
	"$(TEST_HC)" -debug -finfo-table-map -v0 EventlogOutput.hs
	./EventlogOutput +RTS -va 2> EventlogOutput_IPE.stderr.log
	grep "IPE:" EventlogOutput_IPE.stderr.log

.PHONY: T23142
T23142:
	# Test that the -Di output contains different frames
	"$(TEST_HC)" --run -ignore-dot-ghci T23142.hs +RTS -Di -RTS 2> T23142.log
	grep -m1 -c "ATOMICALLY_FRAME" T23142.log
	grep -m1 -c "CATCH_RETRY_FRAME" T23142.log
	grep -m1 -c "CATCH_STM_FRAME" T23142.log
	grep -m1 -c "MUT_ARR_PTRS_FROZEN_DIRTY" T23142.log
	grep -m1 -c "SMALL_MUT_ARR_PTRS_FROZEN_DIRTY" T23142.log

HSC2HS_OPTS = --cc="$(TEST_CC)" $(addprefix --cflag=,$(TEST_CC_OPTS)) --ld=$(TEST_CC) $(addprefix --lflag=,$(TEST_CC_OPTS))

IOManager.hs: IOManager.hsc
	'$(HSC2HS)' $(HSC2HS_OPTS) $<
