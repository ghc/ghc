#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1996/11/21 16:48:45 simonm Exp $

# Todo: generalise this to use HSprog.mk or something.

TOP=../../..
HaskellSuffixRules = YES
include $(TOP)/ghc/mk/ghc.mk

PROGRAMS =  graph hpView hpView2

HC_OPTS = -hi-diffs -fglasgow-exts -fhaskell-1.3 -O -L/usr/X11/lib -cpp
CC_OPTS = -ansi -I/usr/X11/include

# ToDo: use AC_PATH_X in configure to get lib/include dirs for X.

OBJS_graph   = Graph.o           HaskXLib.o
OBJS_hpView  = HpView.o  Parse.o HaskXLib.o
OBJS_hpView2 = HpView2.o Parse.o HaskXLib.o

all :: $(PROGRAMS)

graph : $(OBJS_graph)
	$(HC) -o $@ $(HCFLAGS) $(LDOPTIONS) $(OBJS_graph) -lX11

hpView : $(OBJS_hpView)
	$(HC) -o $@ $(HCFLAGS) $(LDOPTIONS) $(OBJS_hpView) -lX11

hpView2 : $(OBJS_hpView2)
	$(HC) -o $@ $(HCFLAGS) $(LDOPTIONS) $(OBJS_hpView2) -lX11

HaskXLib.o : HaskXLib.c
	$(CC) -c $(CFLAGS) HaskXLib.c

install :: $(PROGRAMS)
	$(INSTALL) $(INSTBINFLAGS) graph   $(DESTDIR)
	$(INSTALL) $(INSTBINFLAGS) hpView  $(DESTDIR)
	$(INSTALL) $(INSTBINFLAGS) hpView2 $(DESTDIR)

clean ::
	$(RM) $(PROGRAMS)

HS_DEP_SRCS = Graph.lhs HpView.lhs HpView2.lhs Parse.lhs
include $(TOP)/mk/hsdepend.mk

tags 	:: $(HS_DEP_SRCS)
	$(HSTAGS) $(HSTAGSFLAGS) $(HS_DEP_SRCS)

# DO NOT DELETE: Beginning of Haskell dependencies
Graph.o : Graph.lhs
HpView.o : HpView.lhs
HpView.o : ./Parse.hi
HpView2.o : HpView2.lhs
HpView2.o : ./Parse.hi
Parse.o : Parse.lhs
# DO NOT DELETE: End of Haskell dependencies
