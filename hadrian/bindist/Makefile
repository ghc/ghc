MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

# Configuration from the source distribution's configure script.
include ./mk/project.mk

# Configuration from the binary distribution's configure script.
include ./config.mk

.PHONY: default
default:
	@echo 'Run "make install" to install'
	@false

#-----------------------------------------------------------------------
# INSTALL RULES

# Hacky function to check equality of two strings
# TODO : find if a better function exists
eq=$(and $(findstring $(1),$(2)),$(findstring $(2),$(1)))

# patchpackageconf
#
# Hacky function to patch up the 'haddock-interfaces' and 'haddock-html'
# fields in the package .conf files
#
# $1 = package name (ex: 'bytestring')
# $2 = path to .conf file
# $3 = Docs Directory
# $4 = (relative) path from $${pkgroot} to docs directory ($3)
# $5 = package name and version (ex: bytestring-0.13)
#
define patchpackageconf \
	# We fix the paths to haddock files by using the relative path from the pkgroot
	# to the doc files.
	cat '$2' | sed 's|haddock-interfaces.*|haddock-interfaces: "$${pkgroot}/$4/html/libraries/$5/$1.haddock"|' \
	         | sed 's|haddock-html.*|haddock-html: "$${pkgroot}/$4/html/libraries/$5"|' \
		 | sed 's|    $${pkgroot}/../../doc/html/.*||' \
	       > '$2.copy'
	# The rts package doesn't actually supply haddocks, so we stop advertising them
	# altogether.
	((echo "$1" | grep rts) && (cat '$2.copy' | sed 's|haddock-.*||' > '$2.copy.copy')) || (cat '$2.copy' > '$2.copy.copy')
	# We finally replace the original file.
	mv '$2.copy.copy' '$2'
	# Fix the mode, in case umask is set
	chmod 644 '$2'
endef

# QUESTION : should we use shell commands?

.PHONY: show
show:
	@echo '$(VALUE)="$($(VALUE))"'


.PHONY: install_extra
ifeq "$(EnableDistroToolchain)" "NO"
install_extra: install_mingw
else
install_extra:
endif

.PHONY: install_bin
ifeq "$(RelocatableBuild)" "YES"
install_bin: install_bin_direct
else
install_bin: install_bin_libdir install_wrappers
endif

.PHONY: install
install: install_bin install_lib install_extra
install: install_man install_docs update_package_db
install: install_data

ifeq "$(RelocatableBuild)" "YES"
ActualLibsDir=${ghclibdir}
ActualBinsDir=${bindir}
else
ActualLibsDir=${ghclibdir}/lib
ActualBinsDir=${ghclibdir}/bin
endif
WrapperBinsDir=${bindir}

# N.B. this is duplicated from includes/ghc.mk.
lib/settings : config.mk
	@rm -f $@
	@echo '[("C compiler command", "$(SettingsCCompilerCommand)")' >> $@
	@echo ',("C compiler flags", "$(SettingsCCompilerFlags)")' >> $@
	@echo ',("C++ compiler command", "$(SettingsCxxCompilerCommand)")' >> $@
	@echo ',("C++ compiler flags", "$(SettingsCxxCompilerFlags)")' >> $@
	@echo ',("C compiler link flags", "$(SettingsCCompilerLinkFlags)")' >> $@
	@echo ',("C compiler supports -no-pie", "$(SettingsCCompilerSupportsNoPie)")' >> $@
	@echo ',("CPP command", "$(SettingsCPPCommand)")' >> $@
	@echo ',("CPP flags", "$(SettingsCPPFlags)")' >> $@
	@echo ',("Haskell CPP command", "$(SettingsHaskellCPPCommand)")' >> $@
	@echo ',("Haskell CPP flags", "$(SettingsHaskellCPPFlags)")' >> $@
	@echo ',("JavaScript CPP command", "$(SettingsJavaScriptCPPCommand)")' >> $@
	@echo ',("JavaScript CPP flags", "$(SettingsJavaScriptCPPFlags)")' >> $@
	@echo ',("C-- CPP command", "$(SettingsCmmCPPCommand)")' >> $@
	@echo ',("C-- CPP flags", "$(SettingsCmmCPPFlags)")' >> $@
	@echo ',("C-- CPP supports -g0", "$(SettingsCmmCPPSupportsG0)")' >> $@
	@echo ',("ld supports compact unwind", "$(LdHasNoCompactUnwind)")' >> $@
	@echo ',("ld supports filelist", "$(LdHasFilelist)")' >> $@
	@echo ',("ld supports single module", "$(LdHasSingleModule)")' >> $@
	@echo ',("ld is GNU ld", "$(LdIsGNULd)")' >> $@
	@echo ',("Merge objects command", "$(SettingsMergeObjectsCommand)")' >> $@
	@echo ',("Merge objects flags", "$(SettingsMergeObjectsFlags)")' >> $@
	@echo ',("Merge objects supports response files", "$(MergeObjsSupportsResponseFiles)")' >> $@
	@echo ',("ar command", "$(SettingsArCommand)")' >> $@
	@echo ',("ar flags", "$(ArArgs)")' >> $@
	@echo ',("ar supports at file", "$(ArSupportsAtFile)")' >> $@
	@echo ',("ar supports -L", "$(ArSupportsDashL)")' >> $@
	@echo ',("ranlib command", "$(SettingsRanlibCommand)")' >> $@
	@echo ',("otool command", "$(SettingsOtoolCommand)")' >> $@
	@echo ',("install_name_tool command", "$(SettingsInstallNameToolCommand)")' >> $@
	@echo ',("windres command", "$(SettingsWindresCommand)")' >> $@
	@echo ',("unlit command", "$$topdir/../bin/$(CrossCompilePrefix)unlit")' >> $@
	@echo ',("cross compiling", "$(CrossCompiling)")' >> $@
	@echo ',("target platform string", "$(TARGETPLATFORM)")' >> $@
	@echo ',("target os", "$(HaskellTargetOs)")' >> $@
	@echo ',("target arch", "$(HaskellTargetArch)")' >> $@
	@echo ',("target word size", "$(TargetWordSize)")' >> $@
	@echo ',("target word big endian", "$(TargetWordBigEndian)")' >> $@
	@echo ',("target has GNU nonexec stack", "$(TargetHasGnuNonexecStack)")' >> $@
	@echo ',("target has .ident directive", "$(TargetHasIdentDirective)")' >> $@
	@echo ',("target has subsections via symbols", "$(TargetHasSubsectionsViaSymbols)")' >> $@
	@echo ',("target has libm", "$(TargetHasLibm)")' >> $@
	@echo ',("Unregisterised", "$(GhcUnregisterised)")' >> $@
	@echo ',("LLVM target", "$(LLVMTarget)")' >> $@
	@echo ',("LLVM llc command", "$(SettingsLlcCommand)")' >> $@
	@echo ',("LLVM opt command", "$(SettingsOptCommand)")' >> $@
	@echo ',("LLVM llvm-as command", "$(SettingsLlvmAsCommand)")' >> $@
	@echo ',("Use inplace MinGW toolchain", "$(SettingsUseDistroMINGW)")' >> $@
	@echo
	@echo ',("Use interpreter", "$(GhcWithInterpreter)")' >> $@
	@echo ',("Support SMP", "$(GhcWithSMP)")' >> $@
	@echo ',("RTS ways", "$(GhcRTSWays)")' >> $@
	@echo ',("Tables next to code", "$(TablesNextToCode)")' >> $@
	@echo ',("Leading underscore", "$(LeadingUnderscore)")' >> $@
	@echo ',("Use LibFFI", "$(UseLibffiForAdjustors)")' >> $@
	@echo ',("RTS expects libdw", "$(GhcRtsWithLibdw)")' >> $@
	@echo ',("Relative Global Package DB", "package.conf.d")' >> $@
	@echo "]" >> $@

# We need to install binaries relative to libraries.
BINARIES = $(wildcard ./bin/*)
.PHONY: install_bin_libdir
install_bin_libdir:
	@echo "Copying binaries to $(DESTDIR)$(ActualBinsDir)"
	$(INSTALL_DIR) "$(DESTDIR)$(ActualBinsDir)"
	for i in $(BINARIES); do \
		if test -L "$$i"; then \
			cp -RP "$$i" "$(DESTDIR)$(ActualBinsDir)"; \
		else \
			$(INSTALL_PROGRAM) "$$i" "$(DESTDIR)$(ActualBinsDir)"; \
		fi; \
	done

.PHONY: install_bin_direct
install_bin_direct:
	@echo "Copying binaries to $(DESTDIR)$(WrapperBinsDir)"
	$(INSTALL_DIR) "$(DESTDIR)$(WrapperBinsDir)"
	$(INSTALL_PROGRAM) ./bin/* "$(DESTDIR)$(WrapperBinsDir)/"

.PHONY: install_lib
install_lib: lib/settings
	@echo "Copying libraries to $(DESTDIR)$(ActualLibsDir)"
	$(INSTALL_DIR) "$(DESTDIR)$(ActualLibsDir)"

	@dest="$(DESTDIR)$(ActualLibsDir)"; \
	cd lib; \
	for i in `$(FIND) . -type f`; do \
		dir="`dirname $$i`" ; \
		$(INSTALL_DIR) "$$dest/$$dir" ; \
		case $$i in \
		  *.a) \
		    $(INSTALL_DATA) $$i "$$dest/$$dir" ; \
		    $(RANLIB_CMD) "$$dest"/$$i ;; \
		  *.dll) \
		    $(INSTALL_PROGRAM) $$i "$$dest/$$dir" ; \
		    $(STRIP_CMD) "$$dest"/$$i ;; \
		  *.so) \
		    $(INSTALL_SHLIB) $$i "$$dest/$$dir" ;; \
		  *.dylib) \
		    $(INSTALL_SHLIB) $$i "$$dest/$$dir" ;; \
		  *.mjs) \
		    $(INSTALL_SCRIPT) $$i "$$dest/`dirname $$i`" ;; \
		  *) \
		    $(INSTALL_DATA) $$i "$$dest/`dirname $$i`" ;; \
		esac; \
	done; \

.PHONY: install_docs
install_docs:
	@echo "Copying docs to $(DESTDIR)$(docdir)"
	$(INSTALL_DIR) "$(DESTDIR)$(docdir)"

	if [ -d doc ]; then \
		cd doc; $(FIND) . -type f -exec sh -c \
			'$(INSTALL_DIR) "$(DESTDIR)$(docdir)/`dirname $$1`" && $(INSTALL_DATA) "$$1" "$(DESTDIR)$(docdir)/`dirname $$1`"' \
			sh '{}' ';'; \
	fi

	if [ -d docs-utils ]; then \
		$(INSTALL_DIR) "$(DESTDIR)$(docdir)/html/libraries/"; \
		$(INSTALL_DATA) docs-utils/prologue.txt "$(DESTDIR)$(docdir)/html/libraries/"; \
		$(INSTALL_SCRIPT) docs-utils/gen_contents_index "$(DESTDIR)$(docdir)/html/libraries/"; \
	fi

.PHONY: install_data
install_data:
	@echo "Copying data to $(DESTDIR)share"
	$(INSTALL_DIR) "$(DESTDIR)$(datadir)"
	if [ -d share ]; then \
		cd share; $(FIND) . -type f -exec sh -c \
			'$(INSTALL_DIR) "$(DESTDIR)$(datadir)/`dirname $$1`" && \
			$(INSTALL_DATA) "$$1" "$(DESTDIR)$(datadir)/`dirname $$1`"' \
			sh '{}' ';'; \
	fi

MAN_SECTION := 1
MAN_PAGES := manpage/ghc.1

.PHONY: install_man
install_man:
	if [ -f $(MAN_PAGES) ]; then \
		$(INSTALL_DIR) "$(DESTDIR)$(mandir)"; \
		$(INSTALL_DIR) "$(DESTDIR)$(mandir)/man$(MAN_SECTION)"; \
		$(INSTALL_MAN) $(INSTALL_OPTS) $(MAN_PAGES) "$(DESTDIR)$(mandir)/man$(MAN_SECTION)"; \
	fi

export SHELL
.PHONY: install_wrappers
install_wrappers: install_bin_libdir install_hsc2hs_wrapper
	@echo "Installing wrapper scripts"
	$(INSTALL_DIR) "$(DESTDIR)$(WrapperBinsDir)"
	for p in `cd wrappers; $(FIND) . ! -type d`; do \
	    mk/install_script.sh "$$p" "$(DESTDIR)/$(WrapperBinsDir)/$$p" "$(WrapperBinsDir)" "$(ActualBinsDir)" "$(ActualBinsDir)/$$p" "$(ActualLibsDir)" "$(docdir)" "$(includedir)"; \
	done

.PHONY: install_hsc2hs_wrapper
install_hsc2hs_wrapper:
	@echo Copying hsc2hs wrapper
	cp mk/hsc2hs wrappers/hsc2hs-ghc-$(ProjectVersion)

PKG_CONFS = $(shell find "$(DESTDIR)$(ActualLibsDir)/package.conf.d" -name '*.conf' | sed "s:   :\0xxx\0:g")
.PHONY: update_package_db
update_package_db: install_bin install_lib
	@echo "Installing C++ standard library virtual package"
	$(INSTALL_DATA) mk/system-cxx-std-lib-1.0.conf "$(DESTDIR)$(ActualLibsDir)/package.conf.d"
	@echo "Updating the package DB"
	$(foreach p, $(PKG_CONFS),\
		$(call patchpackageconf,$(shell echo $(notdir $p) | sed 's/-[0-9.]*-[0-9a-zA-Z]*\.conf//g'),$(shell echo "$p" | sed 's:\0xxx\0:   :g'),$(docdir),$(shell mk/relpath.sh "$(ActualLibsDir)" "$(docdir)"),$(shell echo $(notdir $p) | sed 's/.conf//g')))
	'$(DESTDIR)$(ActualBinsDir)/$(CrossCompilePrefix)ghc-pkg' --global-package-db "$(DESTDIR)$(ActualLibsDir)/package.conf.d" recache

.PHONY: install_mingw
install_mingw:
	@echo "Installing MingGW"
	$(INSTALL_DIR) "$(DESTDIR)$(prefix)/mingw"
	cp -Rp ./mingw "$(DESTDIR)$(prefix)"

# END INSTALL
# ----------------------------------------------------------------------
