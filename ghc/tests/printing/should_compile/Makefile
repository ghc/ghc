TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

HC_OPTS += -noC -ddump-tc -dcore-lint -hi

print002_HC_OPTS = -fno-implicit-prelude -dppr-user -ddump-rdr

include $(TOP)/mk/target.mk

