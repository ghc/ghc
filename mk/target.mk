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
#	clean* distclean* mostlyclean* maintainer-clean*
#	tags*
#	info dvi ps
#	dist binary-dist
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

depend :: $(MKDEPENDHS_SRCS) $(MKDEPENDC_SRCS)
	@$(RM) .depend
	@touch .depend
ifneq "$(DOC_SRCS)" ""
	$(MKDEPENDLIT) -o .depend $(MKDEPENDLIT_OPTS) $(filter %.lit,$(DOC_SRCS))
endif
ifneq "$(MKDEPENDC_SRCS)" ""
	$(MKDEPENDC) -f .depend $(MKDEPENDC_OPTS) \
                -- $(CC_OPTS) -- $(MKDEPENDC_SRCS)
endif
ifneq "$(MKDEPENDHS_SRCS)" ""
ifeq ($(notdir $(MKDEPENDHS)),ghc)
#	New way of doing dependencies: the ghc driver knows how to invoke script
	$(MKDEPENDHS) -M -optdep-f -optdep.depend \
		$(foreach way,$(WAYS),-optdep-s -optdep$(way)) \
		$(MKDEPENDHS_OPTS) \
		$(HC_OPTS) \
		$(MKDEPENDHS_SRCS)
else
#	Old way: call mkdependHS-1.2
	$(MKDEPENDHS) -f .depend $(MKDEPENDHS_OPTS) \
		$(foreach way,$(WAYS),-s $(way)) \
		-- $(HC_OPTS) -- $(MKDEPENDHS_SRCS)
endif
endif


##################################################################
# 			boot
#
#  The boot target, at a minimum generates dependency information

.PHONY: boot
boot :: depend


##################################################################
# 		GNU Standard targets
#
#	Every Makefile should define the following targets
# 
# `all'
#      Compile the entire program. This should be the default target.
#      This target need not rebuild any documentation files; Info files
#      should normally be included in the distribution, and DVI files
#      should be made only when explicitly asked for.
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
#      The way to install Info files is to copy them into `$(infodir)'
#      with $(INSTALL_DATA) (see Command Variables), and then run the
#      install-info program if it is present.  install-info is a script
#      that edits the Info `dir' file to add or update the menu entry for
#      the given Info file; it will be part of the Texinfo package. Here
#      is a sample rule to install an Info file:
# 
# 	     $(infodir)/foo.info: foo.info # There may be a newer info
# 	     file in . than in srcdir.
# 		     -if test -f foo.info; then d=.; \
# 		      else d=$(srcdir); fi; \ $(INSTALL_DATA)
# 		     $$d/foo.info $@; \ # Run install-info only if it
# 	     exists.  # Use `if' instead of just prepending `-' to the
# 	     # line so we notice real errors from install-info.  # We
# 	     use `$(SHELL) -c' because some shells do not # fail
# 	     gracefully when there is an unknown command.
# 		     if $(SHELL) -c 'install-info --version' \
# 			>/dev/null 2>&1; then \ install-info
# 		       --infodir=$(infodir) $$d/foo.info; \ else true;
# 		     fi
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
#      produced by Bison, tags tables, Info files, and so on.
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
# `info'
#      Generate any Info files needed. The best way to write the rules is
#      as follows:
# 
# 	     info: foo.info
# 
# 	     foo.info: foo.texi chap1.texi chap2.texi
# 		     $(MAKEINFO) $(srcdir)/foo.texi
# 
#      You must define the variable MAKEINFO in the Makefile. It should
#      run the makeinfo program, which is part of the Texinfo
#      distribution.
# 
# `dvi' `ps'
#      Generate DVI files for all TeXinfo documentation. For example:
# 
# 	     dvi: foo.dvi
# 
# 	     foo.dvi: foo.texi chap1.texi chap2.texi
# 		     $(TEXI2DVI) $(srcdir)/foo.texi
# 
#      You must define the variable TEXI2DVI in the Makefile. It should
#      run the program texi2dvi , which is part of the Texinfo
#      distribution. Alternatively, write just the dependencies, and
#      allow GNU Make to provide the command.
#
#      ps is a FPtools addition for Postscript files
# 
# `dist' `binary-dist'
#      Create a distribution tar file for this program. The tar file
#      should be set up so that the file names in the tar file start with
#      a subdirectory name which is the name of the package it is a
#      distribution for. This name can include the version number.
# 
#      For example, the distribution tar file of GCC version 1.40 unpacks
#      into a subdirectory named `gcc-1.40'.
# 
#      The easiest way to do this is to create a subdirectory
#      appropriately named, use ln or cp to install the proper files in
#      it, and then tar that subdirectory.
# 
#      The dist target should explicitly depend on all non-source files
#      that are in the distribution, to make sure they are up to date in
#      the distribution. See Making Releases.
#
#	binary-dist is an FPtools addition for binary distributions
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
#      (FPTOOLS: we don't use the suggested script, but rather the
#       mkdirhier script in glafp_utils -- SOF)




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

$(HS_PROG) :: $(HS_OBJS)
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(HS_OBJS) $(LIBS)
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

ifneq "$(LIBRARY)" ""
all :: $(LIBRARY)


define BUILD_LIB
$(RM) $@
$(AR) $(AR_OPTS) $@ $(LIBOBJS)
$(RANLIB) $@
endef

#
# For Haskell object files, we might have chosen to split
# up the object files. Test for whether the library being
# built is consisting of Haskell files by (hackily) checking
# whether HS_SRCS is empty or not.
#

ifneq "$(HS_SRCS)" ""
ifneq "$(filter -split-objs,$(HC_OPTS))" ""
define BUILD_LIB
$(RM) $@
TMPDIR=$(TMPDIR); export TMPDIR; find $(patsubst %.$(way_)o,%,$(LIBOBJS)) -name '*.$(way_)o' -print | xargs ar q $@
$(RANLIB) $@
endef
endif # $(filter...
endif

$(LIBRARY) :: $(LIBOBJS)
	$(BUILD_LIB)
endif

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
SCRIPT_SUBST=$(foreach val,$(SCRIPT_SUBST_VARS),"echo \"$$\"\"$(val)=\\\"$($(val))\\\";\" >> $@;")

all :: $(SCRIPT_PROG)

$(SCRIPT_PROG) :: $(SCRIPT_OBJS)
	$(RM) $@
	@echo Creating $@...
ifneq "$(INTERP)" ""
	@echo "#!"$(INTERP) > $@
else
	@touch $@
endif
ifneq "$(SCRIPT_PREFIX_FILES)" ""
	@cat $(SCRIPT_PREFIX_FILES) >> $@
endif
	@eval $(SCRIPT_SUBST) 
	@cat $(SCRIPT_OBJS) >> $@
	@chmod a+x $@
endif


###########################################
#
#	Targets: install install-strip uninstall
#
###########################################

# For each of these variables that is defined, you
# get one install rule
#
#	INSTALL_PROGS 	 install these executable programs in $(bindir)
#	INSTALL_LIBS	 install these platform-dependent libraries in $(libdir)
#	INSTALL_LIBEXECS install these platform-dependent execs in $(libdir)
#	INSTALL_DATAS	 install these platform-independent files in $(datadir)
#
# If the installation directory variable is undefined, the install rule simply
# emits a suitable error message.
#
# Remember, too, that the installation directory variables ($(bindir) and
# friends can be overridden from their original settings in mk/config.mk.in
# || mk/build.mk
#
.PHONY: install installdirs install-strip install-dirs uninstall install-docs

#
# Sometimes useful to separate out the creation of install directories 
# from the installation itself.
#
installdirs ::
	$(INSTALL_DIR) $(bindir)
	$(INSTALL_DIR) $(libdir)
	$(INSTALL_DIR) $(libexecdir)
	$(INSTALL_DIR) $(datadir)

# Better do this first...
install:: installdirs

ifneq "$(INSTALL_PROGS)" ""
install:: $(INSTALL_PROGS)
	$(INSTALL_PROGRAM) $(INSTALL_OPTS) $(INSTALL_PROGS) $(bindir)
endif

ifneq "$(INSTALL_LIBS)" ""
install:: $(INSTALL_LIBS)
	$(INSTALL_DATA) $(INSTALL_OPTS) $(INSTALL_LIBS) $(libdir)
endif

ifneq "$(INSTALL_LIBEXECS)" ""
install:: $(INSTALL_LIBEXECS)
	$(INSTALL_PROGRAM) $(INSTALL_OPTS) $(INSTALL_LIBEXECS) $(libexecdir)
endif

ifneq "$(INSTALL_DATAS)" ""
install:: $(INSTALL_DATAS)
	$(INSTALL_DATA) $(INSTALL_OPTS) $(INSTALL_DATAS) $(datadir)
endif

#
# Use with care..
#
uninstall:: 
ifeq ($(INSTALL_PROGS),)
	@for i in $(INSTALL_PROGS) ; do				\
		echo rm -f $(bindir)/`basename $$i`;		\
		rm -f $(bindir)/`basename $$i`;			\
	done
endif
ifeq ($(INSTALL_LIBS),)
	@for i in $(INSTALL_LIBS); do				\
		echo rm -f $(libdir)/`basename $$i`;		\
		rm -f $(libdir)/`basename $$i`;			\
	done
endif
ifeq ($(INSTALL_LIBEXECS),)
	@for i in $(INSTALL_LIBEXECS); do			\
		echo rm -f $(libexecdir)/`basename $$i`;	\
		rm -f $(libexecdir)/`basename $$i`;		\
	done
endif
ifeq ($(INSTALL_DATAS),)
	@for i in $(INSTALL_DATAS); do				\
		echo rm -f $(datadir)/`basename $$i`;		\
		rm -f $(datadir)/`basename $$i`;		\
	done
endif

#
# install-strip is from the GNU Makefile standard.
#
ifneq "$(way)" ""
install-strip::
	@$(MAKE) EXTRA_INSTALL_OPTS='-s' install                                	
endif

###########################################
#
#	Targets: dist binary-dist
#
###########################################


#
# dist-pre is a canned rule the toplevel of your source tree
# would use as follows, 
#
#  dist :: dist-pre
#
# it performs two tasks, first creating the distribution directory
# tree and it then decorates the new tree with symbolic links pointing
# to the symbolic links in the build tree.
#
# The dist-pre relies on (at least) the `find' in GNU findutils
# (only tested with version 4.1). All non-GNU `find's I have
# laid on my hands locally, has a restrictive treatment of {} in
# -exec commands, i.e.,
#
#   find . -print echo a{} \;
#   
# does not expand the {}, it has to be a separate argument (i.e. `a {}').
# GNU find is (IMHO) more sensible here, expanding any {} it comes across
# inside an -exec, whether it is a separate argument or part of a word:
#
#  $ touch yes
#  $ find --version
#    GNU find version 4.1
#  $ find yes -exec echo oh,{}! \;
#    oh,yes!
#
# I'm not claiming that the above is not possible to achieve with
# other finds, just that GNU find does the Patently Right Thing here :)
#
# ====> if you're using these dist rules, get hold of GNU findutils.
#
#  --SOF 2/97
#
.PHONY: dist dist-pre dist-post

dist-pre::
	-rm -rf $(SRC_DIST_DIR)
	-rm -f $(SRC_DIST_NAME).tar.gz
	(cd $(FPTOOLS_TOP_ABS); find $(SRC_DIST_DIRS) -type d \( -name CVS -prune -o -name SRC -prune -o -exec $(MKDIRHIER) $(SRC_DIST_DIR)/{} \; \) ; )
	(cd $(FPTOOLS_TOP_ABS); find $(SRC_DIST_DIRS) -name CVS -prune -o -name SRC -prune -o -name "*~" -prune -o -name ".cvsignore" -prune -o -type l -exec $(LN_S) $(FPTOOLS_TOP_ABS)/{} $(SRC_DIST_DIR)/{} \; )

#
# After having created a shadow distribution tree and copied/linked
# all the necessary files to it, `dist-post' makes sure the permissions
# are set right and packages up the tree.
#
# For now, we make the packaging a separate rule, so as to allow
# the inspection of the dist tree before eventually packaging it up.
#
dist-post::
	( cd $(SRC_DIST_DIR) ; cd .. ; chmod -R a+rw $(SRC_DIST_NAME) ) 

dist-package::
	cd $(SRC_DIST_DIR); cd ..; $(TAR) chzf $(SRC_DIST_NAME).tar.gz $(SRC_DIST_NAME)

#
# The default dist rule:
#
# copy/link the contents of $(SRC_DIST_FILES) into the
# shadow distribution tree. SRC_DIST_FILES contain the
# build-generated files that you want to include in
# a source distribution.
#
#
ifneq "$(SRC_DIST_FILES)" ""
dist::
	@for i in $(SRC_DIST_FILES); do 		 \
	  if (test -f "$$i"); then 			 \
	    echo $(LN_S) `pwd`/$$i $(SRC_DIST_DIR)/$$i ; \
	    $(LN_S) `pwd`/$$i $(SRC_DIST_DIR)/$$i ;	 \
	  fi;						 \
	done;
endif

#
# binary-dist creates a binary bundle, set BIN_DIST_NAME
# to package name and do `make binary-dist' (normally this
# just a thing you would do from the toplevel of fptools or)
# from the top of a project.
#
.PHONY: binary-dist-pre binary-dist binary-pack

binary-dist-pre::
	-rm -rf $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)
	-rm -f $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME).tar.gz
	@for i in $(BIN_DIST_DIRS); do 		 	 \
	  if (test -d "$$i"); then 			 \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i-$(ProjectVersion) \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i-$(ProjectVersion) \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share/$$i-$(ProjectVersion) \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share/$$i-$(ProjectVersion) \
	   echo $(MAKE) -C $$i $(MFLAGS) install BIN_DIST=1 BIN_DIST_NAME=$(BIN_DIST_NAME) prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i-$(ProjectVersion) libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i-$(ProjectVersion) datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share/$$i-$(ProjectVersion); \
	   $(MAKE) -C $$i $(MFLAGS) install BIN_DIST=1 BIN_DIST_NAME=$(BIN_DIST_NAME) prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i-$(ProjectVersion) libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i-$(ProjectVersion) datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share/$$i-$(ProjectVersion); \
	  fi; \
	done

#
# Do this separately for now
# 
binary-pack::
	( cd $(BIN_DIST_TMPDIR); $(TAR) chzf $(BIN_DIST_NAME).tar.gz $(BIN_DIST_NAME); rm -rf $(BIN_DIST_NAME) )

###########################################
#
#	Targets: check tags show info
#
###########################################

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

.PHONY: TAGS

TAGS:: $(SOURCES)
	@$(RM) TAGS
	@touch TAGS
ifneq "$(HS_SRCS)" ""
	$(HSTAGS) $(HSTAGS_OPTS) -- $(HS_SRCS)
endif
ifneq "$(C_SRCS)" ""
	etags -a $(C_SRCS)
endif

#------------------------------------------------------------
# 			Makefile debugging
# to see the effective value used for a Makefile variable, do
#  make show VALUE=MY_VALUE
#

show:
	@echo '$(VALUE)=$($(VALUE))'

#------------------------------------------------------------
# 			Documentation

.PHONY: dvi ps html info txt

info:: $(filter %.texinfo, $(DOC_SRCS)) $(filter %.texi,$(DOC_SRCS))
dvi:: $(DOC_DVI)
ps::  $(DOC_PS)
html:: $(DOC_HTML)
texi:: $(DOC_TEXI)
txt:: $(DOC_TEXT)

#
# Building literate root documents requires extra treatment,
# as the root files need to be processed different from other
# literate files (`compile' them into .itex with the -S (standalone)
# option) and then link together a master TeX document with
# a -S option.
#
$(filter %.tex,$(patsubst %.lit,%.tex,$(DOC_SRCS))) :
	@$(RM) $@
	$(LIT2LATEX) -S -c $(LIT2LATEX_OPTS) -o $(patsubst %.tex,%.itex,$@) $(addsuffix .lit,$(basename $@))
	$(LIT2LATEX) -S $(LIT2LATEX_OPTS) -o $@ $(addsuffix .itex,$(basename $@))
	@chmod 444 $@
#
# Ditto for texi and html
#
$(filter %.texi,$(patsubst %.lit,%.texi,$(DOC_SRCS))) :
	@$(RM) $@
	$(LIT2TEXI) -S -c $(LIT2TEXI_OPTS) -o $(patsubst %.texi,%.itxi,$@) $(addsuffix .lit,$(basename $@))
	$(LIT2TEXI) -S $(LIT2TEXI_OPTS) -o $@ $(addsuffix .itxi,$(basename $@))
	@chmod 444 $@
#
# Rather than using lit2html, we opt for the lit-texi-html route,
# and use texi2html as our HTML backend.
# (Note: we need to change mkdependlit to get this really off the ground)
#
# If the generated html representation is split up into a myriad of files,
# put the files in a subdirectory html/, if a monolith is created, park
# the generated file in the same dir as the .lit file.
#
$(filter %.html,$(patsubst %.lit,%.html,$(DOC_SRCS))) : $(filter %.lit,$(DOC_SRCS))
	$(RM) $@ $(patsubst %.html,%.texi,$@) $(patsubst %.html,%.itxi,$@)
ifneq "$(filter -monolithic,$(TEXI2HTML_OPTS))" ""
	$(LIT2TEXI) -S -c $(LIT2TEXI_OPTS) -o $(patsubst %.html,%.itxi,$@) $(addsuffix .lit,$(basename $@))
	$(LIT2TEXI) -S $(LIT2TEXI_OPTS) -o $(patsubst %.html,%.texi,$@) $(addsuffix .itxi,$(basename $@))
	$(TEXI2HTML) $(TEXI2HTML_OPTS) $(patsubst %.html,%.texi,$@)
	cp $(TEXI2HTML_PREFIX)invisible.xbm .
else
	$(RM) html/$(basename $@)*
	$(MKDIRHIER) html
	$(LIT2TEXI) -S -c $(LIT2TEXI_OPTS) -o $(patsubst %.html,%.itxi,$@) $(addsuffix .lit,$(basename $@))
	$(LIT2TEXI) -S $(LIT2TEXI_OPTS) -o html/$(patsubst %.html,%.texi,$@) $(addsuffix .itxi,$(basename $@))
	(cd html; ../$(TEXI2HTML) $(TEXI2HTML_OPTS) $(patsubst %.html,%.texi,$@); cd ..)
	cp $(TEXI2HTML_PREFIX)invisible.xbm html/
	@touch $@
endif
###########################################
#
#	Targets: clean
#
###########################################

.PHONY: realclean mostlyclean clean distclean maintainer-clean

# realclean is just a synonym for maintainer-clean
realclean: maintainer-clean


ifneq "$(MOSTLY_CLEAN_FILES)" ""
mostlyclean::
	rm -f $(MOSTLY_CLEAN_FILES)
endif

ifneq "$(CLEAN_FILES)" ""
clean:: mostlyclean
	rm -f $(CLEAN_FILES)
endif


ifneq "$(DIST_CLEAN_FILES)" ""
distclean:: mostlyclean clean
	rm -f $(DIST_CLEAN_FILES)
endif


ifneq "$(MAINTAINER_CLEAN_FILES)" ""
maintainer-clean:: mostlyclean clean distclean
	@echo 'This command is intended for maintainers to use; it'
	@echo 'deletes files that may need special tools to rebuild.'
	rm -f $(MAINTAINER_CLEAN_FILES)
endif

#
# If (Haskell) object files are split, cleaning up 
# consist of descending into the directories where
# the myriads of object files have been put.
#

ifneq "$(HS_OBJS)" ""
ifneq "$(filter -split-objs,$(HC_OPTS))" ""
clean ::
	find $(patsubst %.$(way_)o,%,$(HS_OBJS)) -name '*.$(way_)o' -print | xargs $(RM) __rm_food;
endif
endif


#################################################################################
#
#			Way management
#
#################################################################################

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
# $(suffix $@) will be .p_o
# The sed script extracts the "p" part.

$(WAY_TARGETS) :
	$(MAKE) way=$(basename $(subst _,.,$(subst .,,$(suffix $@)))) $@

# $(@F) will be something like libHS_p.a, or Foo_p
# $(basename $(@F)) will be libHS_p, or Foo_p
# The sed script extracts the "p" part.

$(LIB_WAY_TARGETS) :
	$(MAKE) $(MFLAGS) $@ way=$(subst .,,$(suffix $(subst _,.,$(basename $@))))

endif	# if way


##################################################################
#
# 		Recursive stuff
#
##################################################################

# Here are the diabolically clever rules that
# 
# (a) for each "recursive target" <t>
#     propagates "make <t>" to directories in SUBDIRS
#
# (b) when SUBDIRS is empty,
#     for each "multi-way-target" <t>
#     calls "make -way=w <t>" for each w in $(WAYS)
#
#     This has the effect of making the standard target
#     in each of the specified ways (as well as in the normal way

# Controlling variables
#	WAYS    = extra (beyond the normal way) ways to build things in
# 	SUBDIRS = subdirectories to recurse into

# No ways, so iterate over the SUBDIRS

ifeq "$(way)" ""
ifneq "$(SUBDIRS)" ""

all docs runtests boot TAGS clean veryclean maintainer-clean install info ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac;
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Recursively making \`$@' in $(SUBDIRS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"
	@for i in $(SUBDIRS) ; do \
	  echo "------------------------------------------------------------------------"; \
	  echo "==fptools== $(MAKE) $@;"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(MFLAGS) $@; \
	done
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Finished making \`$@' in $(SUBDIRS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"

dist ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MKDIRHIER_PREFIX)mkdirhier $(SRC_DIST_DIR)/$$i; \
	  $(MAKE) -C $$i $(MFLAGS) $@ SRC_DIST_DIR=$(SRC_DIST_DIR)/$$i; \
	done
endif
endif

#
# Selectively building subdirectories.
#
#
ifneq "$(SUBDIRS)" ""
$(SUBDIRS) ::
	  $(MAKE) -C $@ $(MFLAGS)
endif

ifneq "$(WAYS)" ""
ifeq "$(way)" ""

# NB: the targets exclude 
#	boot info TAGS
# since these are way-independent
all docs runtests TAGS clean veryclean maintainer-clean install ::
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Recursively making \`$@' for ways: $(WAYS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"
	@for i in $(WAYS) ; do \
	  echo "------------------------------------------------------------------------"; \
	  echo "==fptools== $(MAKE) way=$$i $@;"; \
	  echo "PWD = $(shell pwd)"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) way=$$i --no-print-directory $(MFLAGS) $@ ; \
	done
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Finished recusrively making \`$@' for ways: $(WAYS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"

endif
endif

