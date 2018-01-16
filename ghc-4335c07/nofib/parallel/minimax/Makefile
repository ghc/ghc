TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRCS = Board.hs \
       Game.hs \
		 Prog.hs \
		 Tree.hs \
		 Wins.hs \
		 Main.hs
HC_OPTS += -package parallel -package random

PROG_ARGS = 4 6

include $(TOP)/mk/target.mk

