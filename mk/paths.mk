#################################################################################
#
#			    paths.mk
#
# 	This file defines Make variables for standard directories
#	and file lists
#
#################################################################################


#-----------------------------------------------------------------
#
# Installation setup
#

#-----------------------------------------------------------------------------
# install configuration
#
# The install standard target is guided by the following four variables
#
#  INSTALL_PROGS    (installed in $(bindir))
#  INSTALL_LIBS     (installed in $(libdir))
#  INSTALL_LIBEXECS (installed in $(libexecdir))
#  INSTALL_DATAS    (installed in $(datadir))
#
# see target.mk for more information.
#

#
# Setting user/group ownership for the installed entities
#
# this stuff about "who" does the installing doesn't have make vars
# as it is not intended to be run-time changeable.
#
ifneq "$(OWNER)" ""
INSTALL_OWNER 	= -o $(OWNER)
else
INSTALL_OWNER	=
endif

ifneq "$(GROUP)" ""
INSTALL_GROUP	= -g $(GROUP)
else
INSTALL_GROUP	=
endif

SRC_INSTALL_OPTS += $(INSTALL_OWNER) $(INSTALL_GROUP)

#
# Invocations of `install' for the three different classes
# of targets:
#

INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA    = $(INSTALL) -m 644
INSTALL_DIR     = $(FPTOOLS_TOP)/glafp-utils/mkdirhier/mkdirhier

#
# The install variables does not have any defaults,
# what files to install have to be specified in the Makefiles.
# 
#INSTALL_PROGS += $(HS_PROG) $(C_PROG)
#INSTALL_LIBS  += $(LIBRARY)
#INSTALL_DATAS += $(HS_IFACES)

#-----------------------------------------------------------------------------
# Man pages & Documentation
#
# (AFAIK, not currently used)
#

#man1dir		= $(mandir)/man1
#manext          = '.1'
#man1ext          = '.1'
#man2dir		= $(mandir)/man2
#man2ext          = '.2'
#man3dir		= $(mandir)/man3
#man3ext          = '.3'
#man4dir		= $(mandir)/man4
#man4ext          = '.4'
#man5dir		= $(mandir)/man5
#man5ext          = '.5'
#man6dir		= $(mandir)/man6
#man6ext          = '.6'
#man7dir		= $(mandir)/man7
#man7ext          = '.7'
#man8dir		= $(mandir)/man8
#man8ext          = '.8'

## top of manual page tree to install in
## NB: not necessarily the same place as to look for man pages (?)
#INSTALL_MAN_ROOT = $(mandir)

## prefix for man page _sources_ (vs .../cat)
#INSTALL_MAN_SRCROOT  = $(INSTALL_MAN_ROOT)/man

## man pages for commands
#INSTALL_COMMAND_DIR = $(INSTALL_MAN_SRCROOT)$(COMMANSUFFIX)

## man pages for library routines
#INSTALL_LIB_MANDIR = $(INSTALL_MAN_SRCROOT)$(LIBMANSUFFIX)

## man pages for system commands
#INSTALL_SYS_MANDIR = $(INSTALL_MAN_SRCROOT)$(SYSMANSUFFIX)

## suffix for installing commands'/etcs' man pages
#COMMANSUFFIX 	= 1
#LIBMANSUFFIX 	= 3
#SYSMANSUFFIX 	= 8

INSTALL_INFO_DIR = $(infodir)


INSTALL_FILE_OPTS =
INSTALL_STRIP	  =
INSTALL_BIN_OPTS  = \
	-m 0755 $(INSTALL_OWNER) $(INSTALL_GROUP) $(INSTALL_FILE_FLAGS) $(INSTALL_STRIP)





#################################################################################
#
#		Standard variable names
#
#################################################################################

#
# The fptools mk setup defines a set of standard names which are used by the standard
# targets provided by mk. One example of this is the use of standard names
# for specifying what files to compile, their intermediate/object code, and
# the name of the final executable. Based on the settings of these variables, the
# standard targets will generate/expand rules that automatically compile and
# link your program.
#
# The general rules:
#   
#   SRCS - sources, might be prefixed to indicate what type of source
#          they are.
#   OBJS - object files (possibly prefixed).
#
#   PROG - name of final executable
#
#
#
# BOOT_SRCS: list of machine generated Haskell modules.
# HS_SRCS:   list of Haskell modules you want to compile.
#             (also use by depend rule).
# HS_OBJS:   list of corresponding object files
# HS_PROG:   program that is ultimately linked.
# HS_IFACES: list of interface files generated
#             (caveat: assuming no funny use of -hisuf and that
#               file name and module name match)

SRCS=$(wildcard *.lhs *.hs *.c *.lc *.prl *.lprl *.lit *.verb)

HS_SRCS=$(filter %.lhs %.hs %.hc,$(SRCS))
#
# Do not include BOOT_SRCS in the HS_SRCS defn above,
# since this will make HS_SRCS always be non-empty,
# which breaks rules like `depend'. Price is that
# you have to define BOOT_SRCS before including
# boilerplate.mk
#
ifneq "$(BOOT_SRCS)" ""
HS_SRCS+=$(BOOT_SRCS)
endif
HS_OBJS=$(addsuffix .$(way_)o,$(basename $(HS_SRCS)))
HS_IFACES=$(addsuffix .$(way_)hi,$(basename $(HS_SRCS)))

C_SRCS=$(filter %.lc %.c,$(SRCS))
C_OBJS=$(addsuffix .$(way_)o,$(basename $(C_SRCS)))

# SCRIPT_SRCS:  list of raw script files (in literate form)
# SCRIPT_OBJS:  de-litted scripts
SCRIPT_SRCS=$(filter %.lprl,$(SRCS))
SCRIPT_OBJS=$(addsuffix .prl,$(basename $(SCRIPT_SRCS)))

OBJS=$(HS_OBJS) $(C_OBJS) $(SCRIPT_OBJS)

#
# Note that as long as you use the standard variables for setting
# which C & Haskell programs you want to work on, you don't have
# to set any of the clean variables - the default should do the Right
# Thing.
#

#------------------------------------------------------------------
#
# make depend defaults
#
# The default set of files for the dependency generators to work on
# is just their source equivalents.
# 
MKDEPENDHS_SRCS=$(HS_SRCS)
MKDEPENDC_SRCS=$(C_SRCS)

#------------------------------------------------------------------
# Clean file make-variables.
#
# The following three variables are used to control
# what gets removed when doing `make clean' 
#
# MOSTLYCLEAN_FILES   object code etc., but not stuff
#                     that is slow to recompile and/or stable
#
# CLEAN_FILES  all files that are created by running make.
#
# MAINTAINER_CLEAN_FILES also clean out machine-generated files
#                        that may require extra tools to create.
#
#
MOSTLY_CLEAN_FILES     += $(HS_OBJS) $(C_OBJS)
CLEAN_FILES            += $(HS_PROG) $(C_PROG) $(SCRIPT_PROG) $(PROG) $(LIBRARY) \
			  $(HS_IFACES) \
			  a.out core
MAINTAINER_CLEAN_FILES += .depend $(BOOT_SRCS)

#
# `Standard' set of files to clean out.
#
MOSTLY_CLEAN_FILES += \
 *.CKP *.ln *.BAK *.bak *.o core a.out errs ,* *.a .emacs_*	     \
 tags TAGS *.ind *.ilg *.idx *.idx-prev *.aux *.aux-prev *.dvi *.log \
 *.toc *.lot *.lof *.blg *.info *.itxi *.itex *.ihtml *.cb

#------------------------------------------------------------------
# Documentation setup.
#
# Documentation is not normally produced via the default target, but
# selectively through a set of standard targets (e.g, dvi, html, etc., see
# target.mk). Here we define the variables (and their default settings),
# that control the operation of these standard targets.
#
# Documentation variables:
#
#  DOC_SRCS  = list of documents you want to generate various
#              forms of documentation from.
#  DOC_DVI   = list of DVI files to generate.
#  DOC_HTML  = list of HTML files to generate
#  DOC_TEXI  = list of TexInfo files to generate
#  DOC_TEXT  = list of simple text files to generate
# 
DOC_SRCS=$(wildcard    *.tex *.lit)
DOC_DVI =$(addsuffix  .dvi,$(basename $(DOC_SRCS)))
DOC_PS  =$(addsuffix   .ps,$(basename $(DOC_SRCS)))
DOC_TEXI=$(addsuffix .texi,$(basename $(DOC_SRCS)))
DOC_HTML=$(addsuffix .html,$(basename $(DOC_SRCS)))
DOC_TEXT=$(addsuffix .txt,$(basename $(DOC_SRCS)))

CLEAN_FILES += $(DOC_TEXT) $(DOC_HTML) $(DOC_TEXI) $(DOC_PS) $(DOC_DVI)

#------------------------------------------------------------------
#
# Distribution setup
# 
# Following variables are used for creating source and binary distributions:
#
#  SRC_DIST_NAME && BIN_DIST_NAME  -- the package names
#
#  SRC_DIST_FILES = list of extra files to include from a build tree into a source
#                   distribution
#  
#  SRC_DIST_DIR  = what the current directory in the source/build tree
#                  maps to in the source distrib. tree being created.
#
SRC_DIST_NAME=$(ProjectNameShort)-$(ProjectVersion)

#
# Binary distributions proceeds as follows:
#
# Fromthe top of a build tree, you do `make binary-dist'. The
# canned rule for this  (in target.mk) will then do a binary
# install to a temporary directory before packaging it all up.
# The following variables guide the binary-dist:
#
#  BIN_DIST_TMPDIR= the absolute path to where the temporary directory
#		    structure of a binary distribution should be created.
#		    [Default: toplevel from which you issue `make binary-dist']
#  BIN_DIST_NAME=   what to call the thing.
#
#  BIN_DIST_DIRS=   at the toplevel, list of directories to descend into when
#		    building the distribution tree.
#  
#  An extra directory variable that is set during bin-dists is $(bindist_top), giving
#  the abs. path to the root of the binary installation tree. (useful when punting
#  stuff like README and ANNOUNCE into a distrib, for instance)
#
#  The layout of a binary distribution is described in the
#  installation documentation.
#


