#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 1997/09/05 09:16:35 simonm Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

YACC_OPTS += -d
C_SRCS = syntax.tab.c lex.c id.c tree.c yyerror.c gen.c main.c
C_PROG = ugen
LIBS = $(FLEX_LIB)

CLEAN_FILES += syntax.tab.c syntax.tab.h lex.c
SRC_CC_OPTS += -O

#
# Include flex & bison output in the source distribution.
#
SRC_DIST_FILES+= lex.c syntax.tab.c syntax.tab.h

include $(TOP)/mk/target.mk

#
# Needed quite early in the booting of the compiler, so
# build it right away.
#
boot :: all
