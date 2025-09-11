TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T21097b:
	'$(GHC_PKG)' recache --package-db pkgdb
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -no-global-package-db -no-user-package-db -package-db pkgdb -v0 Test.hs -no-rts -ddump-mod-map
