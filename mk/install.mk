#
#	install* installcheck installdirs
#       install-docs*
#
#    Some of the above targets have a version that
#    recursively invokes that target in sub-directories.
#    This relies on the importing Makefile setting SUBDIRS
#
#    The recursive targets are marked with a * above
#

##################################################################
# 		GNU Standard targets
#
#	Every Makefile should define the following targets
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



# ---------------------------------------------------------------------------
# Symbolic links

# links to programs: we sometimes install a program as
# <name>-<version> with a link from <name> to the real program.

ifneq "$(LINK)" ""

ifeq "$(LINK_TARGET)" ""
ifneq "$(HS_PROG)" ""
LINK_TARGET = $(HS_PROG)
else
ifneq "$(C_PROG)" ""
LINK_TARGET = $(C_PROG)
else
$(error Cannot deduce LINK_TARGET)
endif
endif
endif

#
# install links to script drivers.
#
install ::
	$(INSTALL_DIR) $(DESTDIR)$(bindir)
	if ( $(PERL) -e '$$fn="$(DESTDIR)$(bindir)/$(LINK)"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   echo "Creating a symbolic link from $(LINK_TARGET) to $(LINK) in $(DESTDIR)$(bindir)"; \
	   $(RM) $(DESTDIR)$(bindir)/$(LINK); \
	   $(LN_S) $(LINK_TARGET) $(DESTDIR)$(bindir)/$(LINK); \
	 else \
	   echo "Creating a symbolic link from $(LINK_TARGET) to $(LINK) in $(DESTDIR)$(bindir) failed: \`$(DESTDIR)$(bindir)/$(LINK)' already exists"; \
	   echo "Perhaps remove \`$(DESTDIR)$(bindir)/$(LINK)' manually?"; \
	   exit 1; \
	 fi;

endif # LINK 


###########################################
#
#	Targets: install install-strip
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
.PHONY: install install-docs installdirs install-strip install-docs show-install

show-install :
	@echo "DESTDIR = $(DESTDIR)"
	@echo "bindir = $(bindir)"
	@echo "libdir = $(libdir)"
	@echo "libexecdir = $(libexecdir)  # by default, same as libdir"
	@echo "datadir = $(datadir)  # unused for ghc project"
	@echo "ifacedir = $(ifacedir)"
	@echo "headerdir = $(headerdir)"
	@echo "includedir = $(includedir)"

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
	$(INSTALL_DIR) $(DESTDIR)$(bindir)
	for i in $(INSTALL_PROGS); do \
		    $(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(DESTDIR)$(bindir) ;  \
                    if test "$(darwin_TARGET_OS)" = "1"; then \
                      sh $(FPTOOLS_TOP)/mk/fix_install_names.sh $(libdir) $(DESTDIR)$(bindir)/$$i ; \
                    fi ; \
	done
endif

#
# Just like INSTALL_PROGS, but prefix with install sites bin/lib/data and
# install without stripping.
#
ifneq "$(strip $(INSTALL_SCRIPTS))" ""
install:: $(INSTALL_SCRIPTS)
	$(INSTALL_DIR) $(DESTDIR)$(bindir)
	for i in $(INSTALL_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(DESTDIR)$(bindir); \
	done
endif

ifneq "$(strip $(INSTALL_LIB_SCRIPTS))" ""
install:: $(INSTALL_LIB_SCRIPTS)
	$(INSTALL_DIR) $(DESTDIR)$(libdir)
	for i in $(INSTALL_LIB_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(DESTDIR)$(libdir); \
	done
endif

ifneq "$(strip $(INSTALL_LIBEXEC_SCRIPTS))" ""
install:: $(INSTALL_LIBEXEC_SCRIPTS)
	$(INSTALL_DIR) $(DESTDIR)$(libexecdir)
	for i in $(INSTALL_LIBEXEC_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(DESTDIR)$(libexecdir); \
	done
endif

ifneq "$(strip $(INSTALL_LIBS))" ""
install:: $(INSTALL_LIBS)
	$(INSTALL_DIR) $(DESTDIR)$(libdir)
	for i in $(INSTALL_LIBS); do \
		case $$i in \
		  *.a) \
		    $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DESTDIR)$(libdir); \
		    $(RANLIB) $(DESTDIR)$(libdir)/`basename $$i` ;; \
		  *.dll) \
		    $(INSTALL_DATA) -s $(INSTALL_OPTS) $$i $(DESTDIR)$(libdir) ;; \
		  *.so) \
		    $(INSTALL_SHLIB) $(INSTALL_OPTS) $$i $(DESTDIR)$(libdir) ;; \
		  *.dylib) \
		    $(INSTALL_SHLIB) $(INSTALL_OPTS) $$i $(DESTDIR)$(libdir); \
		    install_name_tool -id $(DESTDIR)$(libdir)/`basename $$i` $(DESTDIR)$(libdir)/`basename $$i` ;; \
		  *) \
		    $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DESTDIR)$(libdir); \
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
	$(INSTALL_DIR) $(DESTDIR)$(libexecdir)
	-for i in $(INSTALL_LIBEXECS); do \
		$(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(DESTDIR)$(libexecdir); \
                if test "$(darwin_TARGET_OS)" = "1"; then \
                        sh $(FPTOOLS_TOP)/mk/fix_install_names.sh $(libdir) $(DESTDIR)$(libexecdir)/`basename $$i` ; \
                fi ; \
	done
endif

ifneq "$(strip $(INSTALL_DATAS))" ""
install:: $(INSTALL_DATAS)
	$(INSTALL_DIR) $(DESTDIR)$(datadir)
	for i in $(INSTALL_DATAS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DESTDIR)$(datadir); \
	done
endif

ifneq "$(strip $(INSTALL_HEADERS))" ""
install:: $(INSTALL_HEADERS)
	$(INSTALL_DIR) $(DESTDIR)$(headerdir)
	for i in $(INSTALL_HEADERS); do \
		$(INSTALL_HEADER) $(INSTALL_OPTS) $$i $(DESTDIR)$(headerdir); \
	done
endif

ifneq "$(strip $(INSTALL_IFACES))" ""
install:: $(INSTALL_IFACES)
	$(INSTALL_DIR) $(DESTDIR)$(ifacedir)
	for i in $(INSTALL_IFACES); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DESTDIR)$(ifacedir); \
	done
endif

ifneq "$(strip $(INSTALL_IFACES_WITH_DIRS))" ""
install:: $(INSTALL_IFACES_WITH_DIRS)
	$(INSTALL_DIR) $(DESTDIR)$(ifacedir)
	for i in $(INSTALL_IFACES_WITH_DIRS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DESTDIR)$(ifacedir)/`dirname $$i`; \
	done
endif

ifneq "$(strip $(INSTALL_DOCS))" ""
ifneq "$(XMLDocWays)" ""
install-docs:: $(INSTALL_DOCS)
	$(INSTALL_DIR) $(DESTDIR)$(datadir)
	for i in $(INSTALL_DOCS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DESTDIR)$(datadir); \
	done
endif
endif

ifneq "$(strip $(INSTALL_XML_DOC))" ""
binary-dist::
	$(MKDIRHIER) $(BIN_DIST_DIR)/docs/$(XML_DOC)
	cp Makefile $(BIN_DIST_DIR)/docs/$(XML_DOC)

ifneq "$(XMLDocWays)" ""
# TODO: The following could be an entry for an Obfuscated Makefile Contest...
install-docs:: $(foreach i,$(XMLDocWays),$(INSTALL_XML_DOC)$(patsubst %.html-no-chunks,%.html,$(patsubst %.html,%/index.html,.$(i))))

install-docs:: $(foreach i,$(XMLDocWays),install-docs-$i)

install-docs-html:
	$(INSTALL_DIR) $(DESTDIR)$(htmldir)
	$(INSTALL_DIR) $(DESTDIR)$(htmldir)/$(INSTALL_XML_DOC)
	$(CP) $(INSTALL_XML_DOC)/* $(DESTDIR)$(htmldir)/$(INSTALL_XML_DOC)

install-docs-html-no-chunks:
	$(INSTALL_DIR) $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(INSTALL_OPTS) $(INSTALL_XML_DOC).html $(DESTDIR)$(htmldir)
	$(CP) $(FPTOOLS_CSS_ABS) $(DESTDIR)$(htmldir)

install-docs-%:
	$(INSTALL_DIR) $(DESTDIR)$($*dir)
	$(INSTALL_DATA) $(INSTALL_OPTS) $(INSTALL_XML_DOC).$* $(DESTDIR)$($*dir)

binary-dist:: $(foreach i,$(XMLDocWays),$(INSTALL_XML_DOC)$(patsubst %.html-no-chunks,%.html,$(patsubst %.html,%/index.html,.$(i))))

binary-dist:: $(foreach i,$(XMLDocWays),binary-dist-docs-$i)

binary-dist-docs-html:
	cp -r $(INSTALL_XML_DOC) $(BIN_DIST_DIR)/docs/$(XML_DOC)/

binary-dist-docs-html-no-chunks:
	cp $(INSTALL_XML_DOC).html $(BIN_DIST_DIR)/docs/$(XML_DOC)/

binary-dist-docs-%:
	cp $(INSTALL_XML_DOC).$* $(BIN_DIST_DIR)/docs/$(XML_DOC)/
endif
endif

#
# install-strip is from the GNU Makefile standard.
#
ifneq "$(way)" ""
install-strip::
	$(MAKE) EXTRA_INSTALL_OPTS='-s' install
endif

