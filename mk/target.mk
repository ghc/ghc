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
# Pre-compute the list of sources so we don't have to do this 
# multiple times.  See paths.mk.

PRE_SRCS := $(ALL_SRCS)

##################################################################
# Include package building machinery

include $(TOP)/mk/package.mk

###################################################################
# Suffix rules for Haskell, C and literate 

include $(TOP)/mk/suffix.mk

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

ifneq "$(BootingFromHc)" "YES"
PKGCONF_DEP = $(STAMP_PKG_CONF)
endif

depend :: $(MKDEPENDHS_SRCS) $(MKDEPENDC_SRCS) $(PKGCONF_DEP)
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
# `dvi' `ps' `pdf' `html' `rtf' 
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

# can't split objs in way 'u', so we disable it here
ifeq "$(way)" "u"
SplitObjs = NO
endif

ifneq "$(HS_SRCS)" ""
ifeq "$(SplitObjs)" "YES"

SRC_HC_OPTS += -split-objs

# We generate the archive into a temporary file libfoo.a.tmp, then
# rename it at the end.  This avoids the problem that ar may sometimes
# fail, leaving a partially built archive behind.
ifeq "$(ArSupportsInput)" ""
define BUILD_LIB
$(RM) $@ $@.tmp
(echo $(STUBOBJS) $(C_OBJS) $(GC_C_OBJS); $(FIND) $(patsubst %.$(way_)o,%_split,$(HS_OBJS)) -name '*.$(way_)o' -print) | xargs $(AR) $@
$(RANLIB) $@
endef
else
define BUILD_LIB
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

HC_SPLIT_PRE = \
    $(RM) $@; if [ ! -d $(basename $@)_split ]; then mkdir $(basename $@)_split; else \
    $(FIND) $(basename $@)_split -name '*.$(way_)o' -print | xargs $(RM) __rm_food; fi
ifeq "$(GhcWithInterpreter)" "YES"
ifeq "$(LdIsGNULd)" "YES"
# If ld is GNU ld, we can use a linker script to pass the names of the
# input files.  This avoids problems with limits on the length of the
# ld command line, which we run into for large Haskell modules.
HC_SPLIT_POST = \
  ( cd $(basename $@)_split; \
    $(RM) ld.script; \
    touch ld.script; \
    echo "INPUT(" *.$(way_)o ")" >>ld.script; \
    $(LD) -r $(LD_X) -o ../$(notdir $@) ld.script; \
  )
else
HC_SPLIT_POST = \
  ( cd $(basename $@)_split; \
    $(LD) -r $(LD_X) -o ../$(notdir $@) *.$(way_)o; \
  )
endif # LdIsGNULd == YES
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
$(LIBRARY) : $(LIBOBJS)
	$(BUILD_LIB)
endif # LIBRARY = ""

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
	   echo "Creating a symbolic link from $(LINK_TARGET) to $(LINK) in $(bindir)"; \
	   $(RM) $(bindir)/$(LINK); \
	   $(LN_S) $(LINK_TARGET) $(bindir)/$(LINK); \
	 else \
	   echo "Creating a symbolic link from $(LINK_TARGET) to $(LINK) in $(bindir) failed: \`$(bindir)/$(LINK)' already exists"; \
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
#	INSTALL_IFACES	     platform-dependent interface files in $(ifacedir)
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

#
# Setting user/group ownership for the installed entities
#
ifneq "$(INSTALL_OWNER)" ""
SRC_INSTALL_OPTS += -o $(INSTALL_OWNER)
endif
ifneq "$(INSTALL_GROUP)" ""
SRC_INSTALL_OPTS += -g $(INSTALL_GROUP)
endif


ifneq "$(strip $(INSTALL_PROGS))" ""

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
                    if test "$(darwin_TARGET_OS)" = "1"; then \
                      sh $(FPTOOLS_TOP)/mk/fix_install_names.sh $(libdir) $(bindir)/$$i ; \
                    fi ; \
	done
endif

#
# Just like INSTALL_PROGS, but prefix with install sites bin/lib/data and
# install without stripping.
#
ifneq "$(strip $(INSTALL_SCRIPTS))" ""
install:: $(INSTALL_SCRIPTS)
	@$(INSTALL_DIR) $(bindir)
	for i in $(INSTALL_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(bindir); \
	done
endif

ifneq "$(strip $(INSTALL_LIB_SCRIPTS))" ""
install:: $(INSTALL_LIB_SCRIPTS)
	@$(INSTALL_DIR) $(libdir)
	for i in $(INSTALL_LIB_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(libdir); \
	done
endif

ifneq "$(strip $(INSTALL_LIBEXEC_SCRIPTS))" ""
install:: $(INSTALL_LIBEXEC_SCRIPTS)
	@$(INSTALL_DIR) $(libexecdir)
	for i in $(INSTALL_LIBEXEC_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(libexecdir); \
	done
endif

ifneq "$(strip $(INSTALL_LIBS))" ""
install:: $(INSTALL_LIBS)
	@$(INSTALL_DIR) $(libdir)
	for i in $(INSTALL_LIBS); do \
		case $$i in \
		  *.a) \
		    $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(libdir); \
		    $(RANLIB) $(libdir)/`basename $$i` ;; \
		  *.dll) \
		    $(INSTALL_DATA) -s $(INSTALL_OPTS) $$i $(libdir) ;; \
		  *.so) \
		    $(INSTALL_SHLIB) $(INSTALL_OPTS) $$i $(libdir) ;; \
		  *.dylib) \
		    $(INSTALL_SHLIB) $(INSTALL_OPTS) $$i $(libdir); \
		    install_name_tool -id $(libdir)/`basename $$i` $(libdir)/`basename $$i` ;; \
		  *) \
		    $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(libdir); \
		esac; \
	done
endif

ifneq "$(strip $(INSTALL_LIBEXECS))" ""
#
# See above comment next to defn of INSTALL_PROGS for what
# the purpose of this one-liner is.
# 
INSTALL_LIBEXECS := $(foreach p, $(INSTALL_LIBEXECS), $(addsuffix $(subst _,,$(subst __,$(exeext),_$(suffix $(p))_)), $(basename $(p))))

install:: $(INSTALL_LIBEXECS)
	@$(INSTALL_DIR) $(libexecdir)
	-for i in $(INSTALL_LIBEXECS); do \
		$(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(libexecdir); \
                if test "$(darwin_TARGET_OS)" = "1"; then \
                        sh $(FPTOOLS_TOP)/mk/fix_install_names.sh $(libdir) $(libexecdir)/`basename $$i` ; \
                fi ; \
	done
endif

ifneq "$(strip $(INSTALL_DATAS))" ""
install:: $(INSTALL_DATAS)
	@$(INSTALL_DIR) $(datadir)
	for i in $(INSTALL_DATAS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir); \
	done
endif

ifneq "$(strip $(INSTALL_IFACES))" ""
install:: $(INSTALL_IFACES)
	@$(INSTALL_DIR) $(ifacedir)
	for i in $(INSTALL_IFACES); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(ifacedir); \
	done
endif

ifneq "$(strip $(INSTALL_IFACES_WITH_DIRS))" ""
install:: $(INSTALL_IFACES_WITH_DIRS)
	@$(INSTALL_DIR) $(ifacedir)
	for i in $(INSTALL_IFACES_WITH_DIRS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(ifacedir)/`dirname $$i`; \
	done
endif

ifneq "$(strip $(INSTALL_INCLUDES))" ""
install:: $(INSTALL_INCLUDES)
	@$(INSTALL_DIR) $(includedir)
	for i in $(INSTALL_INCLUDES); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(includedir); \
	done
endif

ifneq "$(strip $(INSTALL_DOCS))" ""
ifneq "$(XMLDocWays)" ""
install-docs:: $(INSTALL_DOCS)
	@$(INSTALL_DIR) $(datadir)	
	for i in $(INSTALL_DOCS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir); \
	done
endif
endif

# TODO: The following could be an entry for an Obfuscated Makefile Contest...
ifneq "$(strip $(INSTALL_XML_DOC))" ""
ifneq "$(XMLDocWays)" ""
install-docs:: $(foreach i,$(XMLDocWays),$(INSTALL_XML_DOC)$(patsubst %.html-no-chunks,%.html,$(patsubst %.htmlhelp,%.chm,$(patsubst %.html,%/index.html,.$(i)))))
	@$(INSTALL_DIR) $(datadir)	
	@for i in $(XMLDocWays); do \
		if [ $$i = "html" ]; then \
			$(INSTALL_DIR) $(datadir)/html; \
			$(INSTALL_DIR) $(datadir)/html/$(INSTALL_XML_DOC); \
			echo "( cd $(INSTALL_XML_DOC) && $(CP) * $(datadir)/html/$(INSTALL_XML_DOC) )" ; \
			( cd $(INSTALL_XML_DOC) && $(CP) * $(datadir)/html/$(INSTALL_XML_DOC) ) ; \
		else \
			echo $(INSTALL_DATA) $(INSTALL_OPTS) $(INSTALL_XML_DOC)`echo .$$i | sed s/\.htmlhelp/.chm/ | sed s/\.html-no-chunks/.html/` $(datadir); \
			$(INSTALL_DATA) $(INSTALL_OPTS) $(INSTALL_XML_DOC)`echo .$$i | sed s/\.htmlhelp/.chm/ | sed s/\.html-no-chunks/.html/` $(datadir); \
		fi; \
		if [ $$i = "html-no-chunks" ]; then \
			echo $(CP) $(FPTOOLS_CSS_ABS) $(datadir); \
			$(CP) $(FPTOOLS_CSS_ABS) $(datadir); \
		fi \
	done
endif
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
	$(HSTAGS) $(HSTAGS_OPTS) $(TAGS_HS_SRCS)
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
#			DocBook XML Documentation
#
################################################################################

.PHONY: html html-no-chunks htmlhelp fo dvi ps pdf

ifneq "$(XML_DOC)" ""

all :: $(XMLDocWays)

# multi-file XML document: main document name is specified in $(XML_DOC),
# sub-documents (.xml files) listed in $(XML_SRCS).

ifeq "$(XML_SRCS)" ""
XML_SRCS = $(wildcard *.xml)
endif

XML_HTML           = $(addsuffix /index.html,$(basename $(XML_DOC)))
XML_HTML_NO_CHUNKS = $(addsuffix .html,$(XML_DOC))
XML_HTMLHELP       = $(addsuffix -htmlhelp/index.html,$(basename $(XML_DOC)))
XML_FO             = $(addsuffix .fo,$(XML_DOC))
XML_DVI            = $(addsuffix .dvi,$(XML_DOC))
XML_PS             = $(addsuffix .ps,$(XML_DOC))
XML_PDF            = $(addsuffix .pdf,$(XML_DOC))

$(XML_HTML) $(XML_NO_CHUNKS_HTML) $(XML_FO) $(XML_DVI) $(XML_PS) $(XML_PDF) :: $(XML_SRCS)

html           :: $(XML_HTML)
html-no-chunks :: $(XML_HTML_NO_CHUNKS)
htmlhelp       :: $(XML_HTMLHELP)
fo             :: $(XML_FO)
dvi            :: $(XML_DVI)
ps             :: $(XML_PS)
pdf            :: $(XML_PDF)

CLEAN_FILES += $(XML_HTML_NO_CHUNKS) $(XML_FO) $(XML_DVI) $(XML_PS) $(XML_PDF)

extraclean ::
	$(RM) -rf $(XML_DOC).out $(FPTOOLS_CSS) $(basename $(XML_DOC)) $(basename $(XML_DOC))-htmlhelp

validate ::
	$(XMLLINT) --valid --noout $(XMLLINT_OPTS) $(XML_DOC).xml
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


##################################################################
#
# 		Recursive stuff
#
# This was once at the top of the file, allegedly because it was
# needed for some targets, e.g. when building DLLs in libraries.  But
# since this reason is a little short on information, and I'm having
# trouble with subdirectory builds happening before the current
# directory when building hslibs (bad interaction with including
# _hsc.o files in the cbits lib) so I'm moving the recursive makes to
# the end --SDM 12/12/2001
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
#ifeq "$(way)" ""
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
#endif

#
# Selectively building subdirectories.
#
#
ifneq "$(SUBDIRS)" ""
$(SUBDIRS) ::
	  $(MAKE) -C $@ $(MFLAGS)
endif

# -----------------------------------------------------------------------------
# Further cleaning

# Sometimes we want to clean things only after the recursve cleaning
# has heppened (eg. if the files we're about to remove would affect
# the recursive traversal).

distclean::
	rm -f $(LATE_DIST_CLEAN_FILES)

maintainer-clean::
	rm -f $(LATE_DIST_CLEAN_FILES)

