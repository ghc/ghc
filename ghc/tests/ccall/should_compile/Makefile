TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

SRC_HC_OPTS += -noC -dcore-lint -fglasgow-exts

# Note that these tests are still in a state of flux... don't believe errors
# they report. In fact, these aren't really very good tests at all...

cc001_HC_OPTS = -noC
cc002_HC_OPTS = -noC
cc003_HC_OPTS = -noC
cc004_HC_OPTS = -noC
cc005_HC_OPTS = -fvia-C
cc006_HC_OPTS = -fvia-C -fno-prune-tydecls

include $(TOP)/mk/target.mk
