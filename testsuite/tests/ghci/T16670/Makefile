TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	$(RM) -rf my-odir

T16670_unboxed:
	@echo "~~~~~~~~ testing T16670_unboxed"
	$(MAKE) -s --no-print-directory clean
	mkdir my-odir
	echo ":load T16670_unboxed.hs" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) \
		-v0 -fwrite-interface -odir my-odir
	find . -name T16670_unboxed.o
	test -f my-odir/T16670_unboxed.o

T16670_th:
	@echo "~~~~~~~~ testing T16670_th"
	$(MAKE) -s --no-print-directory clean
	mkdir my-odir
	echo ":load T16670_th.hs" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) \
		-v0 -fno-code -fwrite-interface -odir my-odir
	find . -name T16670_th.o
	test -f my-odir/T16670_th.o
