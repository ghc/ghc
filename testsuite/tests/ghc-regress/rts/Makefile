TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

outofmem-prep::
	'$(TEST_HC)' --make -fforce-recomp outofmem.hs -o outofmem

outofmem::
	@$(MAKE) outofmem-prep >prep.out 2>prep.out
	@ulimit -v 10000000 2>/dev/null; ./outofmem || echo "exit($$?)"

outofmem2-prep::
	'$(TEST_HC)' -rtsopts --make -fforce-recomp outofmem2.hs -o outofmem2

outofmem2::
	@$(MAKE) outofmem2-prep >prep.out 2>prep.out
	@ulimit -v 1000000 2>/dev/null; ./outofmem2 +RTS -M5m -RTS || echo "exit($$?)"

T2615-prep:
	$(RM) libfoo_T2615.so
	'$(TEST_HC)' -fPIC -c libfoo_T2615.c -o libfoo_T2615.o
	'$(TEST_HC)' -shared -no-auto-link-packages libfoo_T2615.o -o libfoo_T2615.so

.PHONY: T4059
T4059:
	$(RM) 4059_c.o 4059.o 4059.hi
	'$(TEST_HC)' -v0 --make 4059 4059_c.c
	./4059

exec_signals-prep:
	$(CC) -o exec_signals_child exec_signals_child.c
	$(CC) -o exec_signals_prepare exec_signals_prepare.c

.PHONY: 4850
4850:
	$(RM) 4850.o 4850.hi 4850$(exeext)
	"$(TEST_HC)" -v0 -rtsopts -debug -threaded --make 4850
	./4850 +RTS -s 2>&1 | grep Task | wc -l
