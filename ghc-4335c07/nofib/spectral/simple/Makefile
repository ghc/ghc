TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Without this we get the C version too!
SRCS = Main.hs

# Main is rather big
Main_HC_OPTS = -H110m

SRC_RUNTEST_OPTS += +RTS -K3m -H20m -RTS

include $(TOP)/mk/target.mk
