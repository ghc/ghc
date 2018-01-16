#################################################################################
#
#			target.mk
#
#		Standard targets for GHC
#
#################################################################################

#
# This file contain three groups of target rules:
#
# 1.  GHC targets
#	depend*
#	runtests*
#
# 2.  GNU standard targets
#	all*
#	install* installcheck installdirs
#       install-docs*
#	clean* distclean* mostlyclean* maintainer-clean*
#	tags*
#	dvi ps (no info) GHC adds: pdf rtf html chm HxS
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
# Pre-compute the list of sources so we don't have to do this 
# multiple times.  See paths.mk.

PRE_SRCS := $(ALL_SRCS)

###################################################################
# Suffix rules for Haskell, C and literate 

include $(TOP)/mk/ghc-suffix.mk

##################################################################
# 		GHC standard targets
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

ifneq "$(BootingFromHc)" "YES"
PKGCONF_DEP = $(STAMP_PKG_CONF)
endif

MKDEPENDHS_FLAGS = -dep-suffix "" -dep-makefile .depend

depend :: $(MKDEPENDHS_SRCS) $(PKGCONF_DEP)
	@$(RM) .depend
	@touch .depend
ifneq "$(DOC_SRCS)" ""
	$(MKDEPENDLIT) -o .depend $(MKDEPENDLIT_OPTS) $(filter %.lit,$(DOC_SRCS))
endif
ifneq "$(MKDEPENDHS_SRCS)" ""
	$(MKDEPENDHS) -M $(MKDEPENDHS_FLAGS) $(foreach obj,$(MKDEPENDHS_OBJ_SUFFICES),-osuf $(obj)) $(MKDEPENDHS_OPTS) $(filter-out -split-objs, $(HC_OPTS)) $(MKDEPENDHS_SRCS)
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
# `dvi' `ps' `pdf' `html' `chm' `HxS' `rtf' 
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
#      (GHC: we use a close relative of the suggested script, situated
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
#
# For details of exactly what rule is generated, see the
# relevant section below

.PHONY: all

#----------------------------------------
#	Haskell programs

ifneq "$(HS_PROG)" ""
all :: $(HS_PROG)

ifneq "$(BootingFromHc)" "YES"
$(HS_PROG) :: $(OBJS)
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(OBJS)
else
# see bootstrap.mk
$(HS_PROG) :: $(OBJS)
	$(CC) -o $@ $(HC_BOOT_CC_OPTS) $(HC_BOOT_LD_OPTS) $(OBJS) $(HC_BOOT_LIBS)
endif
endif

#----------------------------------------
#	C programs

ifneq "$(C_PROG)" ""
all :: $(C_PROG)

$(C_PROG) :: $(C_OBJS)
	$(CC) -o $@ $(CC_OPTS) $(LD_OPTS) $(C_OBJS) $(LIBS)
endif

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
define BUILD_STATIC_LIB
$(RM) $@
$(AR) $(AR_OPTS) $@ $(STUBOBJS) $(LIBOBJS)
$(RANLIB) $@
endef
else
define BUILD_STATIC_LIB
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

SRC_HC_OPTS += -split-objs

# We generate the archive into a temporary file libfoo.a.tmp, then
# rename it at the end.  This avoids the problem that ar may sometimes
# fail, leaving a partially built archive behind.
ifeq "$(ArSupportsInput)" ""
define BUILD_STATIC_LIB
$(RM) $@ $@.tmp
(echo $(STUBOBJS) $(C_OBJS) $(GC_C_OBJS); $(FIND) $(patsubst %.$(way_)o,%_split,$(HS_OBJS)) -name '*.$(way_)o' -print) | xargs $(AR) $@
$(RANLIB) $@
endef
else
define BUILD_STATIC_LIB
$(RM) $@ $@.tmp
echo $(STUBOBJS) > $@.list
echo $(C_OBJS) >> $@.list
echo $(GC_C_OBJS) >> $@.list
$(FIND) $(patsubst %.$(way_)o,%_split,$(HS_OBJS)) -name '*.$(way_)o' -print >> $@.list
$(AR) $(AR_OPTS) $@ $(ArSupportsInput) $@.list
$(RM) $@.list
$(RANLIB) $@
endef
endif

# Extra stuff for compiling Haskell files with $(SplitObjs):

#
# If (Haskell) object files are split, cleaning up 
# consist of descending into the directories where
# the myriads of object files have been put.
#

extraclean ::
	$(FIND) $(patsubst %.$(way_)o,%_split,$(HS_OBJS)) -name '*.$(way_)o' -print -o -name ld.script -print | xargs $(RM) __rm_food
	-rmdir $(patsubst %.$(way_)o,%_split,$(HS_OBJS)) > /dev/null 2>&1

endif # $(SplitObjs)
endif # $(HS_SRCS)

#
# Remove local symbols from library objects if requested.
#

ifeq "$(StripLibraries)" "YES"
ifeq "$(SplitObjs)" "YES"
SRC_HC_POST_OPTS += \
  for i in $(basename $@)_split/*.$(way_)o; do \
	$(LD) -r $(LD_X) -o $$i.tmp $$i; \
	$(MV) $$i.tmp $$i; \
  done
else
SRC_HC_POST_OPTS += \
  $(LD) -r $(LD_X) -o $@.tmp $@; $(MV) $@.tmp $@
endif # SplitObjs
endif # StripLibraries

# Note: $(STUBOBJS) isn't depended on here, but included when building the lib.
#       (i.e., the assumption is that $(STUBOBJS) are created as a side-effect
#       of building $(LIBOBJS)).

ifeq "$(LIBRARY:%.so=YES)" "YES"
# ELF styled DSO
$(LIBRARY): $(LIBOBJS) $(LIB_DEPS)
	$(RM) $@
	$(HC) -shared -dynamic -o $@ $(STUBOBJS) $(LIBOBJS) $(LIB_LD_OPTS)
else
ifeq "$(LIBRARY:%.dylib=YES)" "YES"
$(LIBRARY): $(LIBOBJS) $(LIB_DEPS)
	$(HC) -shared -dynamic -o $@ $(STUBOBJS) $(LIBOBJS) $(LIB_LD_OPTS)
else
ifeq "$(LIBRARY:%.dll=YES)" "YES"
#----------------------------------------
#	Building Win32 DLLs
#
$(LIBRARY): $(LIBOBJS) $(LIBRARY).o $(LIB_DEPS)
	$(HC) -shared -dynamic -o $@ $(STUBOBJS) $(LIBOBJS) $(LIBRARY).o $(LIB_LD_OPTS)

DLLTOOL=dlltool

$(LIBRARY).def: $(LIBOBJS)
	$(DLLTOOL) -D $(LIBRARY) --output-def $@ --export-all $(LIBOBJS)

$(LIBRARY).o:
	$(DLLTOOL) -D $(LIBRARY) --output-exp $(LIBRARY).o $(LIBOBJS)

# Generates library.dll.a; by MinGW conventions, this is the dll's import library
$(LIBRARY).a: $(LIBOBJS) $(LIBRARY).def
	$(DLLTOOL) -D $(LIBRARY) --def $(LIBRARY).def --output-lib $@

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
else
# Regular static library
$(LIBRARY): $(LIBOBJS)
	$(BUILD_STATIC_LIB)
endif # %.dll
endif # %.dylib
endif # %.so
endif # LIBRARY = ""

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
	$(HSTAGS) $(HSTAGS_OPTS) $(TAGS_HS_SRCS)
endif
ifneq "$(TAGS_C_SRCS)" ""
	etags -a $(TAGS_C_SRCS)
endif
	@( DEREFFED=`ls -l Makefile | sed -e 's/.*-> \(.*\)/\1/g'` && $(RM) `dirname $$DEREFFED`/TAGS && $(CP) TAGS `dirname $$DEREFFED` ) 2>/dev/null || echo TAGS file generated, perhaps copy over to source tree?

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

include $(TOP)/mk/ghc-recurse.mk

# -----------------------------------------------------------------------------
# Further cleaning

# Sometimes we want to clean things only after the recursve cleaning
# has heppened (eg. if the files we're about to remove would affect
# the recursive traversal).

distclean::
	rm -f $(LATE_DIST_CLEAN_FILES)

maintainer-clean::
	rm -f $(LATE_DIST_CLEAN_FILES)

