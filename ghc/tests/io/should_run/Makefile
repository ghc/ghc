TOP = ../..

include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# io018 should run
OMITTED_RUNTESTS = io005.run io018.run io033.run
endif

include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint
io022_HC_OPTS += -fglasgow-exts
io010_HC_OPTS += -fglasgow-exts
io011_HC_OPTS += -fglasgow-exts
io018_HC_OPTS += -fglasgow-exts
io030_HC_OPTS += -fglasgow-exts
io032_HC_OPTS += -fglasgow-exts

io004_RUNTEST_OPTS += -x 42
io016_RUNTEST_OPTS += io016.hs io016.out
io017_RUNTEST_OPTS += -i io017.stdin
io018_RUNTEST_OPTS += -i io018.hs
io021_RUNTEST_OPTS += -i io021.hs
io022_RUNTEST_OPTS += -i io022.hs
io028_RUNTEST_OPTS += -i io028.hs

.PRECIOUS: %.o %.bin

include $(TOP)/mk/target.mk
