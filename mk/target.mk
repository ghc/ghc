#################################################################################
#
#			target.mk
#
#		Standard targets for fptools
#
#################################################################################

#
# This file contain three groups of target rules:
#
# 1.  FPtools targets
#	depend*
#	runtests*
#
# 2.  GNU standard targets
#	all*
#	install* uninstall installcheck installdirs
#       install-docs*
#	clean* distclean* mostlyclean* maintainer-clean*
#	tags*
#	dvi ps (no info) FPTOOLS adds: pdf rtf html
#	check
#
# 3. Some of the above targets have a version that
#    recursively invokes that target in sub-directories.
#    This relies on the importing Makefile setting SUBDIRS
#
#    The recursive targets are marked with a * above
#

# 
# 
#


##################################################################
#
# 		Recursive stuff
#
# At the top of the file so that recursive makes happen before
# makes in the main directory. This is needed for some targets,
# e.g. when building DLLs in hslibs.
#
##################################################################

# Here are the diabolically clever rules that
# 
# (a) for each "recursive target" <t>
#     propagates "make <t>" to directories in SUBDIRS
#
# (b) when SUBDIRS is empty,
#     for each "multi-way-target" <t>
#     calls "make way=w <t>" for each w in $(WAYS)
#
#     This has the effect of making the standard target
#     in each of the specified ways (as well as in the normal way

# Controlling variables
#	WAYS    = extra (beyond the normal way) ways to build things in
# 	SUBDIRS = subdirectories to recurse into

# No ways, so iterate over the SUBDIRS

# note about recursively invoking make: we'd like make to drop all the
# way back to the top level if it fails in any of the
# sub(sub-...)directories.  This is done by setting the -e flag to the
# shell during the loop, which causes an immediate failure if any of
# the shell commands fail.

# One exception: if the user gave the -i or -k flag to make in the
# first place, we'd like to reverse this behaviour.  So we check for
# these flags, and set the -e flag appropriately.  NOTE: watch out for
# the --no-print-directory flag which is passed to recursive
# invocations of make.
#
ifneq "$(SUBDIRS)" ""

# we override the 'boot', 'all' and 'install' targets in the top
# level Makefile. Some of the sub-projects also set 'boot' to empty.

ifeq "$(NO_ALL_TARGET)" "YES"
ALL_TARGET     =
else
ALL_TARGET     = all
endif

ifeq "$(NO_BOOT_TARGET)" "YES"
BOOT_TARGET    =
else
BOOT_TARGET    = boot
endif

ifeq "$(NO_INSTALL_TARGET)" "YES"
INSTALL_TARGET =
INSTALL_DOCS_TARGET =
else
INSTALL_TARGET = install
INSTALL_DOCS_TARGET = install-docs
endif

$(ALL_TARGET) docs runtests $(BOOT_TARGET) TAGS clean distclean mostlyclean maintainer-clean $(INSTALL_TARGET) $(INSTALL_DOCS_TARGET) html ps dvi txt::
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Recursively making \`$@' in $(SUBDIRS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"
# Don't rely on -e working, instead we check exit return codes from sub-makes.
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(SUBDIRS); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "==fptools== $(MAKE) $@ $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(MFLAGS) $@; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ] ;  then true; else exit 1; fi; \
	done
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Finished making \`$@' in $(SUBDIRS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"

endif

#
# Selectively building subdirectories.
#
#
ifneq "$(SUBDIRS)" ""
$(SUBDIRS) ::
	  $(MAKE) -C $@ $(MFLAGS)
endif

##################################################################
# 		FPtools standard targets
#
# depend:
#
#  The depend target has to cope with a set of files that may have
#  different ways of computing their dependencies, i.e., a Haskell
#  module's dependencies are computed differently from C files.
#
# Note that we don't compute dependencies automatically, i.e., have the
# .depend file be a target that is dependent on the Haskell+C sources,
# and then have the `depend' target depend on `.depend'. The reason for
# this is that when GNU make is processing the `include .depend' statement
# it records .depend as being a Makefile. Before doing any other processing,
# `make' will try to check to see if the Makefiles are up-to-date. And,
# surprisingly enough, .depend has a rule for it, so if any of the source
# files change, it will be invoked, *regardless* of what target you're making.
#
# So, for now, the dependencies has to be re-computed manually via `make depend'
# whenever a module changes its set of imports. Doing what was outlined above
# is only a small optimisation anyway, it would avoid the recomputation of
# dependencies if the .depend file was newer than any of the source modules.
#
.PHONY: depend

# Compiler produced files that are targets of the source's imports.
MKDEPENDHS_OBJ_SUFFICES=o

depend :: $(MKDEPENDHS_SRCS) $(MKDEPENDC_SRCS)
	@$(RM) .depend
	@touch .depend
ifneq "$(DOC_SRCS)" ""
	$(MKDEPENDLIT) -o .depend $(MKDEPENDLIT_OPTS) $(filter %.lit,$(DOC_SRCS))
endif
ifneq "$(MKDEPENDC_SRCS)" ""
	$(MKDEPENDC) -f .depend $(MKDEPENDC_OPTS) $(foreach way,$(WAYS),-s $(way)) -- $(CC_OPTS) -- $(MKDEPENDC_SRCS) 
endif
ifneq "$(MKDEPENDHS_SRCS)" ""
	$(MKDEPENDHS) -M -optdep-f -optdep.depend $(foreach way,$(WAYS),-optdep-s -optdep$(way)) $(foreach obj,$(MKDEPENDHS_OBJ_SUFFICES),-osuf $(obj)) $(MKDEPENDHS_OPTS) $(filter-out -split-objs, $(HC_OPTS)) $(MKDEPENDHS_SRCS)
endif


##################################################################
# 			boot
#
#  The boot target, at a minimum generates dependency information

.PHONY: boot

ifeq "$(NO_BOOT_TARGET)" "YES"
boot ::
else
boot :: depend
endif

##################################################################
# 		GNU Standard targets
#
#	Every Makefile should define the following targets
# 
# `all'
#      Compile the entire program. This should be the default target.
#      This target need not rebuild any documentation files
# 
# `install'
#      Compile the program and copy the executables, libraries, and so on
#      to the file names where they should reside for actual use. If
#      there is a simple test to verify that a program is properly
#      installed, this target should run that test.
# 
#      The commands should create all the directories in which files are
#      to be installed, if they don't already exist. This includes the
#      directories specified as the values of the variables prefix and
#      exec_prefix , as well as all subdirectories that are needed. One
#      way to do this is by means of an installdirs target as described
#      below.
# 
#      Use `-' before any command for installing a man page, so that make
#      will ignore any errors.  This is in case there are systems that
#      don't have the Unix man page documentation system installed.
# 
# `uninstall'
#      Delete all the installed files that the `install' target would
#      create (but not the noninstalled files such as `make all' would
#      create).
# 
# `clean'
# 
#      Delete all files from the current directory that are normally
#      created by building the program.  Don't delete the files that
#      record the configuration. Also preserve files that could be made
#      by building, but normally aren't because the distribution comes
#      with them.
# 
#      Delete `.dvi' files here if they are not part of the
#      distribution.
# 
# `distclean'
#      Delete all files from the current directory that are created by
#      configuring or building the program. If you have unpacked the
#      source and built the program without creating any other files,
#      `make distclean' should leave only the files that were in the
#      distribution.
# 
# `mostlyclean'
#      Like `clean', but may refrain from deleting a few files that
#      people normally don't want to recompile. For example, the
#      `mostlyclean' target for GCC does not delete `libgcc.a', because
#      recompiling it is rarely necessary and takes a lot of time.
# 
# `maintainer-clean'
#      Delete everything from the current directory that can be
#      reconstructed with this Makefile.  This typically includes
#      everything deleted by distclean , plus more: C source files
#      produced by Bison, tags tables, and so on.
# 
#      One exception, however: `make maintainer-clean' should not delete
#      `configure' even if `configure' can be remade using a rule in the
#      Makefile. More generally, `make maintainer-clean' should not delete
#      anything that needs to exist in order to run `configure' and then
#      begin to build the program.
# 
# `TAGS'
#      Update a tags table for this program.
# 
# `dvi' `ps' `pdf' `html' `pdf'
#      Generate DVI/PS/PDF files for LaTeX/DocBook docs. Not everything is
#      supported everywhere, but the intention is to standardise on DocBook
#      producing all formats.
#
# `check'
#      Perform self-tests (if any). The user must build the program
#      before running the tests, but need not install the program; you
#      should write the self-tests so that they work when the program is
#      built but not installed.
# 
# The following targets are suggested as conventional names, for programs
# in which they are useful.
# 
# installcheck
#      Perform installation tests (if any). The user must build and
#      install the program before running the tests. You should not
#      assume that `$(bindir)' is in the search path.
# 
# installdirs
#      It's useful to add a target named `installdirs' to create the
#      directories where files are installed, and their parent
#      directories. There is a script called `mkinstalldirs' which is
#      convenient for this; find it in the Texinfo package.
#      (FPTOOLS: we use a close relative of the suggested script, situated
#       in glafp-utils/mkdirhier -- SOF)




###########################################
#
#	Targets: "all"
#
###########################################

# For each of these variables that is defined
# we generate one "all" rule and one rule for the variable itself:
#
#	HS_PROG		Haskell program
#	C_PROG		C program
#	LIBRARY		Library
#	SCRIPT_PROG	Script (e.g. Perl script)
#
# For details of exactly what rule is generated, see the
# relevant section below

.PHONY: all

#----------------------------------------
#	Haskell programs

ifneq "$(HS_PROG)" ""
all :: $(HS_PROG)

ifneq "$(BootingFromHc)" "YES"
$(HS_PROG) :: $(HS_OBJS)
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(HS_OBJS)
else
# see bootstrap.mk
$(HS_PROG) :: $(HS_OBJS)
	$(CC) -o $@ $(HC_BOOT_CC_OPTS) $(HC_BOOT_LD_OPTS) $(HS_OBJS) $(HC_BOOT_LIBS)
endif

# for building a Haskell program, we add FptoolsHcOpts
SRC_HC_OPTS += $(FptoolsHcOpts)
endif

#----------------------------------------
#	C programs

ifneq "$(C_PROG)" ""
all :: $(C_PROG)

$(C_PROG) :: $(C_OBJS)
	$(CC) -o $@ $(CC_OPTS) $(LD_OPTS) $(C_OBJS) $(LIBS)
endif


#----------------------------------------
#	Building HsLibs libraries.
#
# Inputs:
#   $(PACKAGE) is the name of the library to build
#   $(IS_CBITS_LIB) should be "YES" for a "cbits" library
#
# Outputs:
#   $(LIBRARY)		the name of the library.a
#   $(GHIC_LIBRARY)	the name of the library.o (for GHCi)
#   $(LIBOBJS)		objects to put in library
#   $(STUBOBJS)		more objects to put in library
# 
# $(LIBOBJS) is set to $(HS_OBJS) or $(C_OBJS) depending
# on whether or not it's a "cbits" library.  But you can
# override this by setting $(LIBOBJS) yourself

ifneq "$(PACKAGE)" ""

# add syslib dependencies and current package name
SRC_HC_OPTS += -package-name $(PACKAGE)
SRC_HC_OPTS += $(patsubst %, -package %, $(PACKAGE_DEPS))

ifeq "$(IS_CBITS_LIB)" "YES"
_cbits := _cbits
STUBOBJS += $(HSC_C_OBJS)
# Add _hsc.c files to the cbits library
C_SRCS += $(wildcard ../*_hsc.c)
# Make .hsc.h include files from the directory above visible
# (and the cbits/ library too).
SRC_CC_OPTS += -I.. -I.
endif

ifneq "$(way)" "i"
LIBRARY      = libHS$(PACKAGE)$(_cbits)$(_way).a
GHCI_LIBRARY = HS$(PACKAGE)$(_cbits)$(_way).o
else
LIBRARY      = $(PACKAGE).dll
endif

ifneq "$(IS_CBITS_LIB)" "YES"
WAYS=$(GhcLibWays)
endif

ifeq "$(LIBOBJS)" ""
  ifeq "$(IS_CBITS_LIB)" "YES"
  LIBOBJS = $(C_OBJS)
  else
  LIBOBJS = $(HS_OBJS)
  endif
endif

ifeq "$(IS_CBITS_LIB)" "YES"
override datadir:=$(libdir)/include
INSTALL_DATAS += Hs$(shell perl -e 'print ucfirst "$(PACKAGE)"').h
else
SRC_CC_OPTS += -Icbits
endif

endif # PACKAGE

#----------------------------------------
#	Libraries/archives
#
# Build $(LIBRARY) from $(LIBOBJS)+$(STUBOBJS)
#
# Inputs:
#   $(LIBOBJS)
#   $(STUBOBJS)
#
# Outputs:
#   Rule to build $(LIBRARY)

ifneq "$(LIBRARY)" ""
all :: $(LIBRARY)

ifneq "$(way)" "i"
define BUILD_LIB
$(RM) $@
$(AR) $(AR_OPTS) $@ $(STUBOBJS) $(LIBOBJS)
$(RANLIB) $@
endef
else
define BUILD_LIB
$(RM) $@
al -out:$@ $(STUBOBJS) $(LIBOBJS)
endef
endif

#
# For Haskell object files, we might have chosen to split
# up the object files. Test for whether the library being
# built is consisting of Haskell files by (hackily) checking
# whether HS_SRCS is empty or not.
#

ifneq "$(HS_SRCS)" ""
ifeq "$(SplitObjs)" "YES"

# can't split objs in way 'u', so we disable it here
ifneq "$(way)" "u"

SRC_HC_OPTS += -split-objs

ifeq "$(ArSupportsInput)" ""
define BUILD_LIB
$(RM) $@
(echo $(STUBOBJS); $(FIND) $(patsubst %.$(way_)o,%,$(LIBOBJS)) -name '*.$(way_)o') | xargs ar q $@
$(RANLIB) $@
endef
else
define BUILD_LIB
$(RM) $@
echo $(STUBOBJS) > $@.list
$(FIND) $(patsubst %.$(way_)o,%,$(LIBOBJS)) -name '*.$(way_)o' >> $@.list
$(AR) $(AR_OPTS) $@ $(ArSupportsInput) $@.list
$(RM) $@.list
$(RANLIB) $@
endef
endif

# Extra stuff for compiling Haskell files with $(SplitObjs):

HC_SPLIT_PRE = \
    $(RM) $@; if [ ! -d $(basename $@) ]; then mkdir $(basename $@); else \
    $(FIND) $(basename $@) -name '*.$(way_)o' | xargs $(RM) __rm_food; fi
ifeq "$(GhcWithInterpreter)" "YES"
HC_SPLIT_POST = $(LD) -r $(LD_X) -o $@ $(basename $@)/*.$(way_)o
else
HC_SPLIT_POST = touch $@
endif # GhcWithInterpreter == YES

SRC_HC_PRE_OPTS  += $(HC_SPLIT_PRE);
SRC_HC_POST_OPTS += $(HC_SPLIT_POST);

#
# If (Haskell) object files are split, cleaning up 
# consist of descending into the directories where
# the myriads of object files have been put.
#

extraclean ::
	$(FIND) $(patsubst %.$(way_)o,%,$(HS_OBJS)) -name '*.$(way_)o' -print | xargs $(RM) __rm_food
	-rmdir $(patsubst %.$(way_)o,%,$(HS_OBJS)) > /dev/null 2>&1

endif # $(way) == u
endif # $(SplitObjs)
endif # $(HS_SRCS)

#
# Remove local symbols from library objects if requested.
#

ifeq "$(StripLibraries)" "YES"
ifeq "$(SplitObjs)" "YES"
SRC_HC_POST_OPTS += \
  for i in $(basename $@)/*; do \
	$(LD) -r $(LD_X) -o $$i.tmp $$i; \
	$(MV) $$i.tmp $$i; \
  done
else
SRC_HC_POST_OPTS += \
  $(LD) -r $(LD_X) -o $@.tmp $@; $(MV) $@.tmp $@
endif # SplitObjs
endif # StripLibraries

$(LIBRARY) :: $(STUBOBJS) $(LIBOBJS)
	$(BUILD_LIB)
endif # LIBRARY = ""

#--------------------------------------------------------------
#	Build dynamically-linkable libraries for GHCi
#
# Build $(GHCI_LIBRARY) from $(LIBOBJS)+$(STUBOBJS)
#
# Why?  GHCi can only link .o files (at the moment), not .a files
# so we have to build libFoo.o as well as libFoo.a
#
# Furthermore, GHCi currently never loads 
# profiling libraries (or other non-std ways)
#
# Inputs:
#   $(GHCI_LIBRARY)
#
# Outputs:
#   Rule to build $(GHCI_LIBRARY)


ifneq "$(GHCI_LIBRARY)" ""
ifeq "$(way)" ""
ifeq "$(GhcWithInterpreter)" "YES"


INSTALL_LIBS += $(GHCI_LIBRARY)
CLEAN_FILES += $(GHCI_LIBRARY)

all :: $(GHCI_LIBRARY)

ifneq "$(DONT_WANT_STD_GHCI_LIB_RULE)" "YES"
# If you don't want to build GHCI_LIBRARY the 'standard' way,
# set DONT_WANT_STD_GHCI_LIB_RULE to YES. The Prelude and
# hslibs/Win32 uses this 'feature'.
#
$(GHCI_LIBRARY) :: $(LIBOBJS)
	$(LD) -r $(LD_X) -o $@ $(LIBOBJS) $(STUBOBJS)

endif # DONT_WANT_STD_GHCI_LIB_RULE
endif # GhcWithInterpreter
endif # way
endif # GHCI_LIBRARY != ""


#----------------------------------------
#	Building Win32 DLLs
#

ifeq "$(DLLized)" "YES"
SRC_CC_OPTS += -DDLLized

ifneq "$(PACKAGE)" ""

SRC_BLD_DLL_OPTS += --export-all --output-def=HS$(PACKAGE)$(_cbits)$(_way).def DllVersionInfo.$(way_)o

ifneq "$(PACKAGE) $(IS_CBITS_LIB)" "std YES"
ifneq "$(PACKAGE)" "rts"
SRC_BLD_DLL_OPTS += -lHSstd_cbits_imp -L$(GHC_LIB_DIR)/std/cbits
SRC_BLD_DLL_OPTS += -lHSrts_$(way_)imp -L$(GHC_RUNTIME_DIR)
ifneq "$(PACKAGE)" "std"
  ifeq "$(IS_CBITS_LIB)" ""
  SRC_BLD_DLL_OPTS += -lHSstd_$(way_)imp -L$(GHC_LIB_DIR)/std 
  endif
endif
endif
endif

SRC_BLD_DLL_OPTS += -lgmp -L. -L$(GHC_RUNTIME_DIR)/gmp
ifeq "$(IS_CBITS_LIB)" ""
SRC_BLD_DLL_OPTS += $(patsubst %,-lHS%_$(way_)imp, $(PACKAGE_DEPS))
SRC_BLD_DLL_OPTS += $(patsubst %,-L../%, $(PACKAGE_DEPS))
endif
ifneq "$(HAS_CBITS)" ""
SRC_BLD_DLL_OPTS += -lHS$(PACKAGE)_cbits_imp -Lcbits
endif
SRC_BLD_DLL_OPTS += -lwsock32 -lwinmm

endif # PACKAGE != ""

SplitObjs = NO 

ifneq "$(LIBRARY)" ""

all :: DllVersionInfo.$(way_)o

ifeq "$(DLL_NAME)" ""
DLL_NAME = $(patsubst %.a,%.dll,$(subst lib,,$(LIBRARY)))
endif

ifneq "$(DLL_NAME)" ""
DLL_NAME := $(DLL_PEN)/$(DLL_NAME)
endif

all :: $(DLL_NAME)

ifeq "$(DLL_IMPLIB_NAME)" ""
DLL_IMPLIB_NAME = $(patsubst %.a,%_imp.a,$(LIBRARY))
endif

$(DLL_NAME) :: $(LIBRARY)
	$(BLD_DLL) --output-lib $(DLL_IMPLIB_NAME) -o $(DLL_NAME) $(LIBRARY) $(BLD_DLL_OPTS)
endif # LIBRARY != ""

endif # DLLized

#
# Version information is baked into a DLL by having the DLL include DllVersionInfo.o.
# The version info contains two user tweakables: DLL_VERSION and DLL_VERSION_NAME.
# (both are given sensible defaults though.)
#
# Note: this will not work as expected with Cygwin B20.1; you need a more recent
#       version of binutils (to pick up windres bugfixes.)

ifndef DLL_VERSION
DLL_VERSION=$(ProjectVersion)
endif

ifndef DLL_VERSION_NAME
DLL_VERSION_NAME="http://www.haskell.org/ghc"
endif

ifndef DLL_DESCRIPTION
DLL_DESCRIPTION="A GHC-compiled DLL"
endif

ifndef EXE_VERSION
EXE_VERSION=$(ProjectVersion)
endif

ifndef EXE_VERSION_NAME
EXE_VERSION_NAME="http://www.haskell.org/ghc"
endif

ifndef EXE_DESCRIPTION
EXE_DESCRIPTION="A GHC-compiled binary"
endif

#
# Little bit of lo-fi mangling to get at the right set of settings depending
# on whether we're generating the VERSIONINFO for a DLL or EXE
# 
DLL_OR_EXE=$(subst VersionInfo.$(way_)rc,,$@)
VERSION_FT=$(subst Dll, 0x2L, $(subst Exe, 0x1L, $(DLL_OR_EXE)))
VERSION_RES_NAME=$(subst Exe,$(EXE_VERSION_NAME), $(subst Dll, $(DLL_VERSION_NAME),$(DLL_OR_EXE)))
VERSION_RES=$(subst Exe,$(EXE_VERSION), $(subst Dll, $(DLL_VERSION),$(DLL_OR_EXE)))
VERSION_DESC=$(subst Exe,$(EXE_DESCRIPTION), $(subst Dll, $(DLL_DESCRIPTION),$(DLL_OR_EXE)))

DllVersionInfo.$(way_)rc ExeVersionInfo.$(way_)rc:
	$(RM) DllVersionInfo.$(way_)rc
	echo "1 VERSIONINFO"  		    > $@
	echo "FILEVERSION 1,0,0,1"         >> $@
	echo "PRODUCTVERSION 1,0,0,1"      >> $@
	echo "FILEFLAGSMASK 0x3fL"         >> $@
	echo "FILEOS 0x4L"                 >> $@
	echo "FILETYPE $(VERSION_FT)"      >> $@
	echo "FILESUBTYPE 0x0L"            >> $@
	echo "BEGIN"                       >> $@
	echo " BLOCK \"StringFileInfo\""   >> $@
	echo " BEGIN"                      >> $@
	echo "  BLOCK \"040904B0\""        >> $@
	echo "  BEGIN"                     >> $@
	echo "   VALUE \"CompanyName\", \"$(VERSION_RES_NAME)\\0\"" >> $@
	echo "   VALUE \"FileVersion\", \"$(VERSION_RES)\\0\"" >> $@
	echo "   VALUE \"ProductVersion\", \"$(VERSION_RES)\\0\"" >> $@
	echo "   VALUE \"FileDescription\", \"$(VERSION_DESC)\\0\"" >> $@
	echo "  END" >> $@
	echo " END" >> $@
	echo " BLOCK \"VarFileInfo\""  >> $@
	echo " BEGIN" >> $@
	echo "  VALUE \"Translation\", 0x0409, 1200" >> $@
	echo " END" >> $@
	echo "END" >> $@

#----------------------------------------
#	Script programs

ifneq "$(SCRIPT_PROG)" ""

# To produce a fully functional script, you may
# have to add some configuration variables at the top of 
# the script, i.e., the compiler driver needs to know
# the path to various utils in the build tree for instance.
#
# To have the build rule for the script automatically do this
# for you, set the variable SCRIPT_SUBST_VARS to the list of
# variables you need to put in.

#
# SCRIPT_SUBST creates a string of echo commands that
# will when evaluated append the (perl)variable name and its value 
# to the target it is used for, i.e.,
#
#    A=foo
#    B=bar
#    SCRIPT_SUBST_VARS = A B
#    SCRIPT_SUBST=echo "$""A=\"foo\";" >> $@; echo "$""B=\"bar\";" >> $@
#
#    so if you have a rule like the following
#    
#     foo:
#         @(RM) $@
#         @(TOUCH) $@
#         @eval $(SCRIPT_SUBST)
#
#    `make foo' would create a file `foo' containing the following
#
#    % cat foo
#    $A=foo;
#    $B=bar;
#    %
#
# ToDo: make this work for shell scripts (drop the initial $).
#
ifeq "$(INTERP)" "$(SHELL)"
SCRIPT_SUBST=$(foreach val,$(SCRIPT_SUBST_VARS),"echo \"$(val)=\\\"$($(val))\\\";\" >> $@;")
else
SCRIPT_SUBST=$(foreach val,$(SCRIPT_SUBST_VARS),"echo \"$$\"\"$(val)=\\\"$($(val))\\\";\" >> $@;")
endif

all :: $(SCRIPT_PROG)

$(SCRIPT_PROG) : $(SCRIPT_OBJS)
	$(RM) $@
	@echo Creating $@...
ifeq "$(INTERP)" "perl"
	echo "#! "$(PERL) > $@
else
ifneq "$(INTERP)" ""
	@echo "#!"$(INTERP) > $@
else
	@touch $@
endif
endif
ifneq "$(SCRIPT_PREFIX_FILES)" ""
	@cat $(SCRIPT_PREFIX_FILES) >> $@
endif
ifneq "$(SCRIPT_SUBST)" ""
	@eval $(SCRIPT_SUBST) 
endif
	@cat $(SCRIPT_OBJS) >> $@
	@chmod a+x $@
	@echo Done.
endif

# ---------------------------------------------------------------------------
# Symbolic links

# links to programs: we sometimes install a program as
# <name>-<version> with a link from <name> to the real program.

ifneq "$(LINK)" ""

all :: $(LINK)

CLEAN_FILES += $(LINK)

ifeq "$(LINK_TARGET)" ""
ifneq "$(SCRIPT_PROG)" ""
LINK_TARGET = $(SCRIPT_PROG)
else
ifneq "$(HS_PROG)" ""
LINK_TARGET = $(HS_PROG)
else
ifneq "$(C_PROG)" ""
LINK_TARGET = $(C_PROG)
else
LINK_TARGET = dunno
endif
endif
endif
endif

#
# Don't want to overwrite $(LINK)s that aren't symbolic
# links. Testing for symbolic links is problematic to do in
# a portable fashion using a /bin/sh test, so we simply rely
# on perl.
#
$(LINK) : $(LINK_TARGET)
	@if ( $(PERL) -e '$$fn="$(LINK)"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   echo "Creating a symbolic link from $(LINK_TARGET) to $(LINK)"; \
	   $(RM) $(LINK); \
	   $(LN_S) $(LINK_TARGET) $(LINK); \
	 else \
	   echo "Creating a symbolic link from $(LINK_TARGET) to $(LINK) failed: \`$(LINK)' already exists"; \
	   echo "Perhaps remove \`$(LINK)' manually?"; \
	   exit 1; \
	 fi;


#
# install links to script drivers.
#
install ::
	@$(INSTALL_DIR) $(bindir)
	@if ( $(PERL) -e '$$fn="$(bindir)/$(LINK)"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   echo "Creating a symbol link from $(LINK_TARGET) to $(LINK) in $(bindir)"; \
	   $(RM) $(bindir)/$(LINK); \
	   $(LN_S) $(LINK_TARGET) $(bindir)/$(LINK); \
	 else \
	   echo "Creating a symbol link from $(LINK_TARGET) to $(LINK) in $(bindir) failed: \`$(bindir)/$(LINK)' already exists"; \
	   echo "Perhaps remove \`$(bindir)/$(LINK)' manually?"; \
	   exit 1; \
	 fi;

endif # LINK 


###########################################
#
#	Targets: install install-strip uninstall
#
###########################################

# For each of these variables that is defined, you
# get one install rule
#
#	INSTALL_PROGS 	     executable programs in $(bindir)
#	INSTALL_SCRIPTS	     executable scripts in $(bindir)
#	INSTALL_LIBS	     platform-dependent libraries in $(libdir) (ranlib'ed)
#	INSTALL_LIB_SCRIPTS  platform-dependent scripts   in $(libdir)
#	INSTALL_LIBEXECS     platform-dependent execs in $(libdir)
#	INSTALL_DATAS	     platform-independent files in $(datadir)
#
# If the installation directory variable is undefined, the install rule simply
# emits a suitable error message.
#
# Remember, too, that the installation directory variables ($(bindir) and
# friends can be overridden from their original settings in mk/config.mk.in
# || mk/build.mk
#
.PHONY: install install-docs installdirs install-strip install-dirs uninstall install-docs show-install

show-install :
	@echo "bindir = $(bindir)"
	@echo "libdir = $(libdir)"
	@echo "libexecdir = $(libexecdir)  # by default, same as libdir"
	@echo "datadir = $(datadir)  # unused for ghc project"

#
# Sometimes useful to separate out the creation of install directories 
# from the installation itself.
#
install-dirs ::
	@$(INSTALL_DIR) $(bindir)
	@$(INSTALL_DIR) $(libdir)
	@$(INSTALL_DIR) $(libexecdir)
	@$(INSTALL_DIR) $(datadir)

# Better do this first...
# but we won't for the moment, do it on-demand from
# within the various install targets instead.
#install:: install-dirs

# Install libraries automatically
ifneq "$(LIBRARY)" ""
INSTALL_LIBS  += $(LIBRARY)
ifeq "$(DLLized)" "YES"
INSTALL_PROGS += $(DLL_NAME)
INSTALL_LIBS += $(patsubst %.a,%_imp.a, $(LIBRARY))
endif
INSTALL_DATAS += $(HS_IFACES)
endif

ifneq "$(INSTALL_PROGS)" ""

#
# Here's an interesting one - when using the win32 version
# of install (provided via the cygwin toolkit), we have to
# supply the .exe suffix, *if* there's no other suffix.
#
# The rule below does this by ferreting out the suffix of each
# entry in the INSTALL_PROGS list. If there's no suffix, use
# $(exeext).
# 
INSTALL_PROGS := $(foreach p, $(INSTALL_PROGS), $(addsuffix $(if $(suffix $(p)),,$(exeext)), $(basename $(p))))

install:: $(INSTALL_PROGS)
	@$(INSTALL_DIR) $(bindir)
	@for i in $(INSTALL_PROGS); do \
		    echo $(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(bindir); \
		    $(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(bindir) ;  \
	done
endif

#
# Just like INSTALL_PROGS, but prefix with install sites bin/lib/data and
# install without stripping.
#
ifneq "$(INSTALL_SCRIPTS)" ""
install:: $(INSTALL_SCRIPTS)
	@$(INSTALL_DIR) $(bindir)
	for i in $(INSTALL_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(bindir); \
	done
endif

ifneq "$(INSTALL_LIB_SCRIPTS)" ""
install:: $(INSTALL_LIB_SCRIPTS)
	@$(INSTALL_DIR) $(libdir)
	for i in $(INSTALL_LIB_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(libdir); \
	done
endif

ifneq "$(INSTALL_LIBEXEC_SCRIPTS)" ""
install:: $(INSTALL_LIBEXEC_SCRIPTS)
	@$(INSTALL_DIR) $(libexecdir)
	for i in $(INSTALL_LIBEXEC_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(libexecdir); \
	done
endif

ifneq "$(INSTALL_LIBS)" ""
install:: $(INSTALL_LIBS)
	@$(INSTALL_DIR) $(libdir)
	for i in $(INSTALL_LIBS); do \
		case $$i in \
		  *.a) \
		    $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(libdir); \
		    $(RANLIB) $(libdir)/`basename $$i` ;; \
		  *.dll) \
		    $(INSTALL_DATA) -s $(INSTALL_OPTS) $$i $(libdir) ;; \
		  *) \
		    $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(libdir); \
		esac; \
	done
endif

ifneq "$(INSTALL_LIBEXECS)" ""
#
# See above comment next to defn of INSTALL_PROGS for what
# the purpose of this one-liner is.
# 
INSTALL_LIBEXECS := $(foreach p, $(INSTALL_LIBEXECS), $(addsuffix $(subst _,,$(subst __,$(exeext),_$(suffix $(p))_)), $(basename $(p))))

install:: $(INSTALL_LIBEXECS)
	@$(INSTALL_DIR) $(libexecdir)
	-for i in $(INSTALL_LIBEXECS); do \
		$(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(libexecdir); \
	done
endif

ifneq "$(INSTALL_DATAS)" ""
install:: $(INSTALL_DATAS)
	@$(INSTALL_DIR) $(datadir)
	for i in $(INSTALL_DATAS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir); \
	done
endif

ifneq "$(INSTALL_INCLUDES)" ""
install:: $(INSTALL_INCLUDES)
	@$(INSTALL_DIR) $(includedir)
	for i in $(INSTALL_INCLUDES); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(includedir); \
	done
endif

ifneq "$(INSTALL_DOCS)$(INSTALL_SGML_DOC)" ""
install-docs:: $(INSTALL_DOCS) $(foreach j,$(SGMLDocWays),$(INSTALL_SGML_DOC).$j)
	@$(INSTALL_DIR) $(datadir)
	for i in $(INSTALL_DOCS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir); \
	done
	for i in $(SGMLDocWays); do \
		if [ $$j = "html" ]; then \
			$(CP) -r $(INSTALL_SGML_DOC) $(datadir); \
		else \
			$(INSTALL_DATA) $(INSTALL_OPTS) $(INSTALL_SGML_DOC).$$j $(datadir); \
		fi \
	done
endif

#
# Use with care..
#
uninstall:: 
	@for i in $(INSTALL_PROGS) "" ; do			\
	  if test "$$i"; then 					\
		echo rm -f $(bindir)/`basename $$i`;		\
		rm -f $(bindir)/`basename $$i`;			\
	  fi; 							\
	done
	@for i in $(INSTALL_LIBS) ""; do			\
	  if test "$$i"; then 					\
		echo rm -f $(libdir)/`basename $$i`;		\
		rm -f $(libdir)/`basename $$i`;			\
	  fi;							\
	done
	@for i in $(INSTALL_LIBEXECS) ""; do			\
	  if test "$$i"; then 					\
		echo rm -f $(libexecdir)/`basename $$i`;	\
		rm -f $(libexecdir)/`basename $$i`;		\
	  fi;							\
	done
	@for i in $(INSTALL_DATAS) ""; do			\
	  if test "$$i"; then 					\
		echo rm -f $(datadir)/`basename $$i`;		\
		rm -f $(datadir)/`basename $$i`;		\
	  fi;							\
	done

#
# install-strip is from the GNU Makefile standard.
#
ifneq "$(way)" ""
install-strip::
	@$(MAKE) EXTRA_INSTALL_OPTS='-s' install                                	
endif

##############################################################################
#
#	Targets: check tags show
#
##############################################################################

#------------------------------------------------------------
# 			Check

.PHONY: check

check:: $(TESTS)
	@for i in $(filter-out %.lhs .hs, $(TESTS)) ''; do	\
	  if (test -f "$$i"); then 		\
	    echo Running: `basename $$i` ;	\
	    cd test; `basename $$i` ;		\
	  fi;					\
	done;

#------------------------------------------------------------
# 			Tags

.PHONY: TAGS tags

tags TAGS:: $(TAGS_HS_SRCS) $(TAGS_C_SRCS)
	@$(RM) TAGS
	@touch TAGS
ifneq "$(TAGS_HS_SRCS)" ""
	$(HSTAGS) $(HSTAGS_OPTS) -- $(TAGS_HS_SRCS)
endif
ifneq "$(TAGS_C_SRCS)" ""
	etags -a $(TAGS_C_SRCS)
endif
	@( DEREFFED=`ls -l Makefile | sed -e 's/.*-> \(.*\)/\1/g'` && $(RM) `dirname $$DEREFFED`/TAGS && $(CP) TAGS `dirname $$DEREFFED` ) 2>/dev/null || echo TAGS file generated, perhaps copy over to source tree?

#------------------------------------------------------------
# 			Makefile debugging
# to see the effective value used for a Makefile variable, do
#  make show VALUE=MY_VALUE
#

show:
	@echo '$(VALUE)="$($(VALUE))"'

################################################################################
#
#			SGML Documentation
#
################################################################################

.PHONY: dvi ps html pdf rtf

ifneq "$(SGML_DOC)" ""

all :: $(SGMLDocWays)

# multi-file SGML document: main document name is specified in $(SGML_DOC),
# sub-documents (.sgml files) listed in $(SGML_SRCS).

ifeq "$(SGML_SRCS)" ""
SGML_SRCS = $(wildcard *.sgml)
endif

SGML_TEX  = $(addsuffix .tex,$(SGML_DOC))
SGML_DVI  = $(addsuffix .dvi,$(SGML_DOC))
SGML_PS   = $(addsuffix .ps,$(SGML_DOC))
SGML_PDF  = $(addsuffix .pdf,$(SGML_DOC))
SGML_RTF  = $(addsuffix .rtf,$(SGML_DOC))
SGML_HTML = $(addsuffix .html,$(SGML_DOC))
# HTML output goes in a subdirectory on its own.
SGML_TEXT = $(addsuffix .txt,$(SGML_DOC))

$(SGML_DVI) $(SGML_PS) $(SGML_HTML) $(SGML_TEXT) $(SGML_PDF) :: $(SGML_SRCS)

dvi  :: $(SGML_DVI)
ps   :: $(SGML_PS)
pdf  :: $(SGML_PDF)
rtf  :: $(SGML_RTF)
html :: $(SGML_HTML)
txt  :: $(SGML_TEXT)

CLEAN_FILES += $(SGML_TEXT) $(SGML_TEX) $(SGML_PS) $(SGML_DVI) $(SGML_PDF) $(SGML_RTF) $(SGML_HTML) $(SGML_DOC)-*.html
# can't use $(SGML_SRCS) here, it was maybe used elsewhere

extraclean ::
	$(RM) -rf DBTOHTML_OUTPUT_*
	$(RM) -rf *.junk/
	$(RM) -rf $(SGML_DOC)
endif

##############################################################################
#
#	Targets: clean
#
##############################################################################

# we have to be careful about recursion here; since all the clean
# targets are recursive, we don't want to make eg. distclean depend on
# clean because that would result in far too many recursive calls.

.PHONY: mostlyclean clean distclean maintainer-clean

mostlyclean::
	rm -f $(MOSTLY_CLEAN_FILES)

# extraclean is used for adding actions to the clean target.
extraclean::

clean:: extraclean
	rm -f $(MOSTLY_CLEAN_FILES) $(CLEAN_FILES)

distclean:: extraclean
	rm -f $(MOSTLY_CLEAN_FILES) $(CLEAN_FILES) $(DIST_CLEAN_FILES)

maintainer-clean:: extraclean
	@echo 'This command is intended for maintainers to use; it'
	@echo 'deletes files that may need special tools to rebuild.'
	rm -f $(MOSTLY_CLEAN_FILES) $(CLEAN_FILES) $(DIST_CLEAN_FILES) $(MAINTAINER_CLEAN_FILES)

################################################################################
#
#			Way management
#
################################################################################

# Here is the ingenious jiggery pokery that allows you to build multiple versions
# of a program in a single build tree.
#
# The ways setup requires the following variables to be set:
#
# Expects:	$(WAYS)			the possible "way" strings to one of 
#					which $(way) will be set


# So how does $(way) ever get set to anything?  Answer, we recursively
# invoke make, setting $(way) on the command line.
# When do we do this recursion?  Answer: whenever the programmer
# asks make to make a target that involves a way suffix.
# We must remember *not* to recurse again; but that's easy: we
# just see if $(way) is set:

ifeq "$(way)" ""

# If $(WAYS) = p mc, then WAY_TARGETS expands to
#	%.p_lhs %.p_hs %.p_o ... %.mc_lhs %.p_hs ...
# and OTHER_WAY_TARGETS to
#	%_p.a %_p %_mc.a %_mc
# where the suffixes are from $(SUFFIXES)
#
# We have to treat libraries and "other" targets differently, 
# because their names are of the form
#	libHS_p.a and Foo_p
# whereas everything else has names of the form
#	Foo.p_o

FPTOOLS_SUFFIXES := o hi hc

WAY_TARGETS     = $(foreach way,$(WAYS),$(foreach suffix, $(FPTOOLS_SUFFIXES), %.$(way)_$(suffix)))
LIB_WAY_TARGETS = $(foreach way,$(WAYS),%_$(way).a %_$(way))

# $@ will be something like Foo.p_o
# $(suffix $@)     returns .p_o
# $(subst .,.p_o)  returns p_o
# $(subst _,.,p_o) returns p.o   (clever)
# $(basename p.o)  returns p
# 
$(WAY_TARGETS) :
	$(MAKE) way=$(basename $(subst _,.,$(subst .,,$(suffix $@)))) $@

# $(@F) will be something like libHS_p.a, or Foo_p
# $(basename $(@F)) will be libHS_p, or Foo_p
# The sed script extracts the "p" part.

$(LIB_WAY_TARGETS) :
	$(MAKE) $(MFLAGS) $@ way=$(subst .,,$(suffix $(subst _,.,$(basename $@))))

endif	# if way

# -------------------------------------------------------------------------
# Object and interface files have suffixes tagged with their ways

ifneq "$(way)" ""
SRC_HC_OPTS += -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(way_)o
endif

# -------------------------------------------------------------------------
# Rules to invoke the current target recursively for each way

ifneq "$(WAYS)" ""
ifeq "$(way)" ""

# NB: the targets exclude 
#	boot runtests
# since these are way-independent
all docs TAGS clean distclean mostlyclean maintainer-clean install ::
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Recursively making \`$@' for ways: $(WAYS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"
# Don't rely on -e working, instead we check exit return codes from sub-makes.
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(WAYS) ; do \
	  echo "------------------------------------------------------------------------"; \
	  echo "==fptools== $(MAKE) way=$$i $@;"; \
	  echo "PWD = $(shell pwd)"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) way=$$i --no-print-directory $(MFLAGS) $@ ; \
	  if [ $$? -eq 0 ] ; then true; else exit $$x_on_err; fi; \
	done
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Finished recursively making \`$@' for ways: $(WAYS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"

endif
endif
