TOPDIR = ..
BINARIES = bh bh-prim

bh_DPH = vect
bh_SOURCES = vect.hs BarnesHutVect.hs

bh-prim_SOURCES = BarnesHut.hs BarnesHutPar.hs BarnesHutSeq.hs BarnesHutList.hs BarnesHutGen.hs
bh-prim_FLAGS = -package dph-prim-seq -package dph-prim-par -package dph-base

include $(TOPDIR)/mk/test.mk

