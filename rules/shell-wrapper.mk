# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

define shell-wrapper
$(call trace, shell-wrapper($1,$2))
$(call profStart, shell-wrapper($1,$2))
# $1 = dir
# $2 = distdir

ifeq "$$($1_$2_SHELL_WRAPPER_NAME)" ""
$1_$2_SHELL_WRAPPER_NAME = $1/$$($1_$2_PROGNAME).wrapper
endif

ifeq "$$($1_$2_WANT_INPLACE_WRAPPER)" "YES"

ifeq "$$($1_$2_TOPDIR)" "YES"
INPLACE_WRAPPER = $$(INPLACE_LIB)/$$($1_$2_PROG)
else
INPLACE_WRAPPER = $$(INPLACE_BIN)/$$($1_$2_PROG)
endif

all_$1_$2 : $$(INPLACE_WRAPPER)

$$(INPLACE_BIN)/$$($1_$2_PROG): WRAPPER=$$@
ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"
$$(INPLACE_WRAPPER): $$($1_$2_SHELL_WRAPPER_NAME)
endif
$$(INPLACE_WRAPPER): $$($1_$2_INPLACE)
	$$(call removeFiles,                                                    $$@)
	echo '#!$$(SHELL)'                                                   >> $$@
	echo 'executablename="$$(TOP)/$$<"'                                  >> $$@
	echo 'datadir="$$(TOP)/$$(INPLACE_LIB)"'                             >> $$@
	echo 'bindir="$$(TOP)/$$(INPLACE_BIN)"'                              >> $$@
	echo 'topdir="$$(TOP)/$$(INPLACE_TOPDIR)"'                           >> $$@
	echo 'pgmgcc="$$(WhatGccIsCalled)"'                                  >> $$@
	$$($1_$2_SHELL_WRAPPER_EXTRA)
	$$($1_$2_INPLACE_SHELL_WRAPPER_EXTRA)
ifeq "$$(DYNAMIC_GHC_PROGRAMS)" "YES"
	echo '$$(call prependLibraryPath,$$($1_$2_DEP_LIB_DIRS_SEARCHPATH))' >> $$@
endif
ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"
	cat $$($1_$2_SHELL_WRAPPER_NAME)                                     >> $$@
else
	echo 'exec "$$$$executablename" $$$${1+"$$$$@"}'                     >> $$@
endif
	$$(EXECUTABLE_FILE)                                                     $$@

endif

ifeq "$$($1_$2_WANT_INSTALLED_WRAPPER)" "YES"

ifeq "$$($1_$2_INSTALL_SHELL_WRAPPER_NAME)" ""
$1_$2_INSTALL_SHELL_WRAPPER_NAME = $$($1_$2_PROG)
endif

# Install the binary in $(ghclibexecdir), and install a shell wrapper in $(bindir)
INSTALL_LIBEXECS += $1/$2/build/tmp/$$($1_$2_PROG)
BINDIST_WRAPPERS += $$($1_$2_SHELL_WRAPPER_NAME)

install: install_$1_$2_wrapper

.PHONY: install_$1_$2_wrapper
install_$1_$2_wrapper: WRAPPER=$$(DESTDIR)$$(bindir)/$(CrossCompilePrefix)$$($1_$2_INSTALL_SHELL_WRAPPER_NAME)
install_$1_$2_wrapper:
	$$(call INSTALL_DIR,"$$(DESTDIR)$$(bindir)")
	$$(call removeFiles,                                        "$$(WRAPPER)")
	$$(CREATE_SCRIPT)                                           "$$(WRAPPER)"
	echo '#!$$(SHELL)'                                       >> "$$(WRAPPER)"
	echo 'exedir="$$(ghclibexecdir)/bin"'                    >> "$$(WRAPPER)"
	echo 'exeprog="$$($1_$2_PROG)"'                          >> "$$(WRAPPER)"
	echo 'executablename="$$$$exedir/$$$$exeprog"'           >> "$$(WRAPPER)"
	echo 'datadir="$$(datadir)"'                             >> "$$(WRAPPER)"
	echo 'bindir="$$(bindir)"'                               >> "$$(WRAPPER)"
	echo 'topdir="$$(topdir)"'                               >> "$$(WRAPPER)"
	$$($1_$2_SHELL_WRAPPER_EXTRA)
	$$($1_$2_INSTALL_SHELL_WRAPPER_EXTRA)
	cat $$($1_$2_SHELL_WRAPPER_NAME)                         >> "$$(WRAPPER)"
	$$(EXECUTABLE_FILE)                                         "$$(WRAPPER)"

endif

ifeq "$$($1_$2_WANT_BINDIST_WRAPPER)" "YES"
ifneq "$$(TargetOS_CPP)" "mingw32"

$1_$2_BINDIST_WRAPPER = $1/$2/build/tmp/$$($1_$2_PROGNAME)-bindist

all_$1_$2 : $$($1_$2_BINDIST_WRAPPER)

BINDIST_EXTRAS += $$($1_$2_BINDIST_WRAPPER)

$$($1_$2_BINDIST_WRAPPER): $1/$2/build/tmp/$$($1_$2_PROG)
	$$(call removeFiles,                                                  $$@)
	echo '#!$$(SHELL)'                                                 >> $$@
ifeq "$$(DYNAMIC_GHC_PROGRAMS)" "YES"
	echo '$$(call prependLibraryPath,$$($1_$2_DEP_LIB_REL_DIRS_SEARCHPATH))' >> $$@
endif
	echo 'exec "$$<" $$$${1+"$$$$@"}'                                  >> $$@
	$$(EXECUTABLE_FILE)                                                   $$@

endif
endif

$(call profEnd, shell-wrapper($1,$2))
endef
