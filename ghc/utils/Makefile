TOP=../..
include $(TOP)/ghc/mk/ghc.mk

SUBDIRS = hp2ps		\
	  hscpp		\
	  mkdependHS	\
	  stat2resid	\
	  ugen		\
	  unlit
#	  parallel	\

#	  hstags
#    not ready to go for 2.01

# "heap-view" is not in the list because (a) it requires
#  a Haskell compiler (which you may not have yet), and (b) you are
#  unlikely to want it desperately.  It is easy to build once you have
#  a Haskell compiler and if you want it.

include $(TOP)/mk/subdir.mk
