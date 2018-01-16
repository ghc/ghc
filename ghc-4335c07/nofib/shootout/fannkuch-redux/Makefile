TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

FAST_OPTS = 10
NORM_OPTS = 11
SLOW_OPTS = 12  # official shootout setting

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -XBangPatterns -O2
