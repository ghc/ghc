TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T7476

T7476:
	"$(TEST_HC)" -v0 -ddump-minimal-imports T7476.hs
	cat Main.imports

