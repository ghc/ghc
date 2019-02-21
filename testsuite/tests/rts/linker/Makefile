TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

unsigned_reloc_macho_64:
	as -o unsigned_reloc_macho_64.o unsigned_reloc_macho_64.s
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RTSOPTS) -v0 --make -no-rtsopts-suggestions -no-hs-main -o runner -debug runner.c
	./runner unsigned_reloc_macho_64.o getAnswer
