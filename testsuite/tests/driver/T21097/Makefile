TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T21097:
	'$(GHC_PKG)' recache --package-db pkgdb
	- '$(TEST_HC)' -package-db pkgdb -v0 Test.hs; test $$? -eq 2
