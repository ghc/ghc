TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_RUNTEST_OPTS += -stdout-binary

# Override SRCS so that we don't link in MandelOld.hs
SRCS = Main.lhs Mandel.lhs PortablePixmap.lhs

# fast/norm/slow stuff done by separate stdin files.

include $(TOP)/mk/target.mk

