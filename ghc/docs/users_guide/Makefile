TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(BIN_DIST)" "1"
install :: html dvi info
endif

# Split up the pages on a per-sect2 basis. Big HTML docs
# are a pain to flick through and use.
SRC_SGML2HTML_OPTS += -s 1

SGML_DOC = users_guide

include $(TOP)/mk/target.mk
