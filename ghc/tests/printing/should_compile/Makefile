TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

HC_OPTS += -noC -dcore-lint -hi-with-declarations -hi-with-exports -hi-with-instances -hi-with-fixities -ddump-tc

print002_HC_OPTS = -fno-implicit-prelude

include $(TOP)/mk/target.mk

