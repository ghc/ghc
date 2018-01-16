TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp -package old-time

# kLongLivedTreeDepth	  = 17 :: Int
# kArraySize	 	     = 500000 :: Int
# kMinTreeDepth 	     = 4 :: Int
# kMaxTreeDepth 	     = 17 :: Int

# NORM_OPTS = 17 500000 4 17
NORM_OPTS = 19 500000 5 22

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H180m -RTS
endif

include $(TOP)/mk/target.mk

