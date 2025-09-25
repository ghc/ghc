TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  LDD := otool -L
else
  LDD := ldd
endif


LOCAL_PKGCONF=package.conf.d

clean:
	rm -f test/*.o test/*.hi test/*.a *.o *.hi Hello
	rm -rf $(LOCAL_PKGCONF)

.PHONY: mostly-static
mostly-static:
	@rm -rf $(LOCAL_PKGCONF)
	"$(TEST_CC)" -c "test/test.c" -o "test/test.o"
	"$(AR)" qc "test/libtest-1.0.a" "test/test.o"
	"$(RANLIB)" "test/libtest-1.0.a"
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	"$(GHC_PKG)" --no-user-package-db -f $(LOCAL_PKGCONF) register test/test.pkg -v0
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -static-external -exclude-static-external=c,m,rt,dl,pthread,stdc++,atomic,gmp,ffi,iconv,wsock32,gdi32,winmm,dbghelp,psapi,user32,shell32,mingw32,kernel32,advapi32,mingwex,ws2_32,shlwapi,ole32,rpcrt4,ntdll,ucrt -hide-all-packages -package-db $(LOCAL_PKGCONF)/ -package base -package-id test-1.0 Hello.hs
	./Hello
	$(LDD) Hello | grep libtest || true
