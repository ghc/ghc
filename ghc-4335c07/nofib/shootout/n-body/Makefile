TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

FAST_OPTS = 500000
NORM_OPTS = 5000000
SLOW_OPTS = 50000000  # official shootout setting

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -O2 -XBangPatterns -fexcess-precision
