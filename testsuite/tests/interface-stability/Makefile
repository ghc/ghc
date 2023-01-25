TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

exports_% :
	"$(DUMP_DECLS)" "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" $*
