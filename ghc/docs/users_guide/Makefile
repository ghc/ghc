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

#	release.sgml \
#	3-00-notes.sgml \
#	2-10-notes.sgml \
#	2-09-notes.sgml \
#	2-08-notes.sgml \
#	2-06-notes.sgml \
#	2-04-notes.sgml \
#	2-03-notes.sgml \
#	2-02-notes.sgml \
#	how_to_run.sgml \

SGML_DOC = users_guide

include $(TOP)/mk/target.mk
