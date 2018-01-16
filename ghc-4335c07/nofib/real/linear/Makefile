TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk
# It's necessary to specify the HS_SRCS so that the current make-based system
# compiles the modules in the right order (otherwise it starts with AbsCg, which
# fails due to missing .hi files)
# TODO(michalt): This should go away once we switch to the Shake-based system
HS_SRCS = Utils.lhs Misc.lhs Densematrix.lhs AbsDensematrix.lhs Matrix.lhs Cg.lhs Input.lhs Matlib.lhs Absmatlib.lhs AbsCg.lhs Main.lhs

include $(TOP)/mk/target.mk

