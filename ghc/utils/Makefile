TOP=..
include $(TOP)/mk/boilerplate.mk

ifneq "$(BIN_DIST_NAME)" ""
# We're doing a binary-dist, descend into a subset of the dirs.
SUBDIRS = hp2ps hscpp hstags mkdependHS stat2resid unlit
else
SUBDIRS = hp2ps		\
	  hscpp		\
	  hstags	\
	  mkdependHS	\
	  parallel      \
	  stat2resid	\
	  ugen		\
	  unlit

endif

# "heap-view" is not in the list because (a) it requires
#  a Haskell compiler (which you may not have yet), and (b) you are
#  unlikely to want it desperately.  It is easy to build once you have
#  a Haskell compiler and if you want it.

include $(TOP)/mk/target.mk
