TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

FAST_OPTS = 500
NORM_OPTS = 3000
SLOW_OPTS = 5500  # official shootout setting

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -XBangPatterns -XMagicHash -O2 -fexcess-precision
