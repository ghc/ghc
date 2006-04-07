TOP=../..
include $(TOP)/mk/boilerplate.mk

PROGRAMS = graph hpView hpView2

SRC_HC_OPTS += -hi-diffs -fglasgow-exts -fhaskell-1.3 -O -L/usr/X11/lib -cpp
SRC_CC_OPTS += -ansi -I/usr/X11/include
# ToDo: use AC_PATH_X in configure to get lib/include dirs for X.

OBJS_graph   = Graph.o           HaskXLib.o
OBJS_hpView  = HpView.o  Parse.o HaskXLib.o
OBJS_hpView2 = HpView2.o Parse.o HaskXLib.o

all :: $(PROGRAMS)

graph : $(OBJS_graph)
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(OBJS_graph) -lX11

hpView : $(OBJS_hpView)
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(OBJS_hpView) -lX11

hpView2 : $(OBJS_hpView2)
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(OBJS_hpView2) -lX11

HaskXLib.o : HaskXLib.c
	$(CC) -c $(CC_OPTS) HaskXLib.c

INSTALL_PROGS += $(PROGRAMS)
CLEAN_FILES   += $(PROGRAMS)

include $(TOP)/mk/target.mk
