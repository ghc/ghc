TOP=../..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG=../db2html
SCRIPT_OBJS=db2html.sh
SCRIPT_SUBST_VARS=JADE DOCBOOK_CATALOG HTML_DIR
INTERP=$(SHELL)

HTML_DIR=$(FPTOOLS_TOP_ABS)/glafp-utils/docbook

CLEAN_FILES += $(SCRIPT_PROG)
DESTDIR=$(INSTSCRIPTDIR)

include $(TOP)/mk/target.mk
