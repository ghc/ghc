TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=package.conf.d

clean:
	rm -f test/*.o test/*.hi test/*.a *.o *.hi Hello
	rm -rf $(LOCAL_PKGCONF)

.PHONY: fully-static
fully-static:
	@rm -rf $(LOCAL_PKGCONF)
	"$(TEST_HC)" $(TEST_HC_OPTS) -this-unit-id test-1.0 -c test/Test.hs -staticlib -o test/libtest-1.0.a
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	"$(GHC_PKG)" --no-user-package-db -f $(LOCAL_PKGCONF) register test/test.pkg -v0
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -fully-static -hide-all-packages -package-db $(LOCAL_PKGCONF)/ -package base -package-id test-1.0 Hello.hs
	./Hello
	ldd Hello 2>&1 | grep -q "Not a valid dynamic program"
