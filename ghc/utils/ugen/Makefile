#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1996/11/21 16:49:13 simonm Exp $

TOP=../../..
FlexSuffixRules = YES
YaccSuffixRules = YES
include $(TOP)/ghc/mk/ghc.mk

YFLAGS = -d
SRCS = syntax.tab.c lex.c id.c tree.c yyerror.c gen.c main.c
PROG = ugen
LIBS = $(FLEX_LIB)
DESTDIR = $(INSTBINDIR)

clean ::
	$(RM) lex.c syntax.tab.c syntax.tab.h

include $(TOP)/mk/Cprog.mk
