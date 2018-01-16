# REVISE!!!

TOPDIR = ..
PROGS = mkg concomp
include $(TOPDIR)/mk/test.mk

ALGS = AwShU.hs AwShUP.hs HybU.hs HybUP.hs

mkg.o: Graph.hi
mkg: Graph.o

concomp.o: Graph.hi $(ALGS:.hs=.hi)
concomp: Graph.o $(ALGS:.hs=.o)

