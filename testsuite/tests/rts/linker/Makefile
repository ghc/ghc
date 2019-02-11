TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

unsigned_reloc_macho_x64:
	as -o unsigned_reloc_macho_x64.o unsigned_reloc_macho_x64.s
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RTSOPTS) -v0 --make -no-rtsopts-suggestions -no-hs-main -o runner runner.c
	./runner unsigned_reloc_macho_x64.o getAnswer
