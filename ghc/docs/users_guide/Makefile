TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(BIN_DIST)" "1"
install :: html dvi info
endif

# These files are just concatenated to produce the complete document, in the
# same order as they are given below.

SGML_SRCS = \
	user.sgml \
	intro.sgml \
	3-02-notes.sgml \
	using.sgml \
	  runtime_control.sgml \
	  profiling.sgml \
	  debugging.sgml \
	sooner.sgml \
	lang.sgml \
	  glasgow_exts.sgml \
	  parallel.sgml \
	  vs_haskell.sgml \
	libraries.sgml \
	  posix.sgml \
	  libmisc.sgml \
	gone_wrong.sgml \
	utils.sgml \
	end.sgml

SGML_DOC = users_guide

include $(TOP)/mk/target.mk
