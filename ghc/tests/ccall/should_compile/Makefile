TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

HC_OPTS += -dcore-lint -fglasgow-exts

# Note that these tests are still in a state of flux... don't believe errors
# they report. In fact, these aren't really very good tests at all...

cc001_HC_OPTS = -noC -ddump-tc -ddump-ds 
cc002_HC_OPTS = -noC -ddump-tc -ddump-ds 
cc003_HC_OPTS = -noC -ddump-tc -ddump-ds
cc004_HC_OPTS = -noC -ddump-tc -ddump-ds 
cc005_HC_OPTS = -fvia-C -ddump-stg -ddump-flatC
cc006_HC_OPTS = -fvia-C -ddump-stg -ddump-flatC
cc007_HC_OPTS = -fvia-C -ddump-stg -ddump-flatC

include $(TOP)/mk/target.mk
