#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 1997/03/24 03:02:09 sof Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk
override WAYS=

YACC_OPTS += -d
C_SRCS = syntax.tab.c lex.c id.c tree.c yyerror.c gen.c main.c
C_PROG = ugen
LIBS = $(FLEX_LIB)

CLEAN_FILES += syntax.tab.c syntax.tab.h

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
