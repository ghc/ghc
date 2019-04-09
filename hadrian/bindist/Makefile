MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

include mk/install.mk
include mk/config.mk

.PHONY: default
default:
	@echo 'Run "make install" to install'
	@false

#-----------------------------------------------------------------------
# INSTALL RULES

# Hacky function to check equality of two strings
# TODO : find if a better function exists
eq=$(and $(findstring $(1),$(2)),$(findstring $(2),$(1)))

define installscript
# $1 = package name
# $2 = wrapper path
# $3 = bindir
# $4 = ghcbindir
# $5 = Executable binary path
# $6 = Library Directory
# $7 = Docs Directory
# $8 = Includes Directory
# We are installing wrappers to programs by searching corresponding
# wrappers. If wrapper is not found, we are attaching the common wrapper
# to it. This implementation is a bit hacky and depends on consistency
# of program names. For hadrian build this will work as programs have a
# consistent naming procedure.
	rm -f '$2'
	$(CREATE_SCRIPT) '$2'
	@echo "#!$(SHELL)" >>  '$2'
	@echo "exedir=\"$4\"" >> '$2'
	@echo "exeprog=\"$1\"" >> '$2'
	@echo "executablename=\"$5\"" >> '$2'
	@echo "bindir=\"$3\"" >> '$2'
	@echo "libdir=\"$6\"" >> '$2'
	@echo "docdir=\"$7\"" >> '$2'
	@echo "includedir=\"$8\"" >> '$2'
	@echo "" >> '$2'
	cat wrappers/$1 >> '$2'
	$(EXECUTABLE_FILE) '$2' ;
endef

# Hacky function to patch up the 'haddock-interfaces' and 'haddock-html'
# fields in the package .conf files
define patchpackageconf
#
# $1 = package name (ex: 'bytestring')
# $2 = path to .conf file
# $3 = Docs Directory
# $4 = (relative) path from $${pkgroot} to docs directory ($3)
#
# We fix the paths to haddock files by using the relative path from the pkgroot
# to the doc files.
	cat '$2' | sed 's|haddock-interfaces.*|haddock-interfaces: "$${pkgroot}/$4/html/libraries/$1/$1.haddock"|' \
	         | sed 's|haddock-html.*|haddock-html: "$${pkgroot}/$4/html/libraries/$1"|' \
		 | sed 's|    $${pkgroot}/../../docs/html/.*||' \
	       > '$2.copy'
# The rts package doesn't actually supply haddocks, so we stop advertising them
# altogether.
	((echo "$1" | grep rts) && (cat '$2.copy' | sed 's|haddock-.*||' > '$2.copy.copy')) || (cat '$2.copy' > '$2.copy.copy')
# We finally replace the original file.
	mv '$2.copy.copy' '$2'
endef

# QUESTION : should we use shell commands?


.PHONY: install
install: install_lib install_bin install_includes
install: install_docs install_wrappers install_ghci
install: install_mingw update_package_db

ActualBinsDir=${ghclibdir}/bin
ActualLibsDir=${ghclibdir}/lib
WrapperBinsDir=${bindir}

# We need to install binaries relative to libraries.
BINARIES = $(wildcard ./bin/*)
install_bin:
	@echo "Copying binaries to $(ActualBinsDir)"
	$(INSTALL_DIR) "$(ActualBinsDir)"
	for i in $(BINARIES); do \
		cp -R $$i "$(ActualBinsDir)"; \
	done

install_ghci:
	@echo "Copying and installing ghci"
	$(CREATE_SCRIPT) '$(WrapperBinsDir)/ghci'
	@echo "#!$(SHELL)" >>  '$(WrapperBinsDir)/ghci'
	cat wrappers/ghci-script >> '$(WrapperBinsDir)/ghci'
	$(EXECUTABLE_FILE) '$(WrapperBinsDir)/ghci'

LIBRARIES = $(wildcard ./lib/*)
install_lib:
	@echo "Copying libraries to $(ActualLibsDir)"
	$(INSTALL_DIR) "$(ActualLibsDir)"
	for i in $(LIBRARIES); do \
		cp -R $$i "$(ActualLibsDir)/"; \
	done

INCLUDES = $(wildcard ./include/*)
install_includes:
	@echo "Copying libraries to $(includedir)"
	$(INSTALL_DIR) "$(includedir)"
	for i in $(INCLUDES); do \
		cp -R $$i "$(includedir)/"; \
	done

DOCS = $(wildcard ./docs/*)
install_docs:
	@echo "Copying libraries to $(docdir)"
	$(INSTALL_DIR) "$(docdir)"
	for i in $(DOCS); do \
		cp -R $$i "$(docdir)/"; \
	done

BINARY_NAMES=$(shell ls ./wrappers/)
install_wrappers:
	@echo "Installing Wrapper scripts"
	$(INSTALL_DIR) "$(WrapperBinsDir)"
	$(foreach p, $(BINARY_NAMES),\
		$(call installscript,$p,$(WrapperBinsDir)/$p,$(WrapperBinsDir),$(ActualBinsDir),$(ActualBinsDir)/$p,$(ActualLibsDir),$(docdir),$(includedir)))

PKG_CONFS = $(shell find "$(ActualLibsDir)/package.conf.d" -name '*.conf' | sed 's:   :xxx:g')
update_package_db:
	@echo "$(PKG_CONFS)"
	@echo "Updating the package DB"
	$(foreach p, $(PKG_CONFS),\
		$(call patchpackageconf,$(shell echo $(notdir $p) | sed 's/-\([0-9]*[0-9]\.\)*conf//g'),$(shell echo "$p" | sed 's:xxx:   :g'),$(docdir),$(shell realpath --relative-to="$(libdir)" "$(docdir)")))
	'$(WrapperBinsDir)/ghc-pkg' recache

# The 'foreach' that copies the mingw directory will only trigger a copy
# when the wildcard matches, therefore only on Windows.
MINGW = $(wildcard ./mingw)
install_mingw:
	@echo "Installing MingGW"
	$(INSTALL_DIR) "$(prefix)/mingw"
	$(foreach d, $(MINGW),\
		cp -R ./mingw "$(prefix)")
# END INSTALL
# ----------------------------------------------------------------------
