# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

define shell-wrapper
$(call trace, shell-wrapper($1,$2))
$(call profStart, shell-wrapper($1,$2))
# $1 = dir
# $2 = distdir

ifeq "$$(Windows)" "YES"
$1_$2_WANT_INPLACE_WRAPPER = NO
else ifeq "$$($1_$2_INSTALL_INPLACE)" "NO"
$1_$2_WANT_INPLACE_WRAPPER = NO
else ifeq "$$(DYNAMIC_BY_DEFAULT)" "YES"
# We need to set LD_LIBRARY_PATH for all programs, so always need
# a shell wrapper
$1_$2_WANT_INPLACE_WRAPPER = YES
else ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"
$1_$2_WANT_INPLACE_WRAPPER = YES
else
$1_$2_WANT_INPLACE_WRAPPER = NO
endif

ifeq "$$(Windows)" "YES"
$1_$2_WANT_INSTALLED_WRAPPER = NO
else ifeq "$$($1_$2_INSTALL)" "NO"
$1_$2_WANT_INSTALLED_WRAPPER = NO
else ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"
$1_$2_WANT_INSTALLED_WRAPPER = YES
else
$1_$2_WANT_INSTALLED_WRAPPER = NO
endif


ifeq "$$($1_$2_WANT_INPLACE_WRAPPER)" "YES"

ifeq "$$($1_$2_SHELL_WRAPPER_NAME)" ""
$1_$2_SHELL_WRAPPER_NAME = $1/$$($1_$2_PROG).wrapper
endif

all_$1_$2 : $$(INPLACE_BIN)/$$($1_$2_PROG)

$$(INPLACE_BIN)/$$($1_$2_PROG): WRAPPER=$$@
$$(INPLACE_BIN)/$$($1_$2_PROG): $$($1_$2_INPLACE) $$($1_$2_SHELL_WRAPPER_NAME)
	$$(call removeFiles,                             $$@)
	echo '#!$$(SHELL)'                             >> $$@
	echo 'executablename="$$(TOP)/$$<"'            >> $$@
	echo 'datadir="$$(TOP)/$$(INPLACE_LIB)"'       >> $$@
	echo 'bindir="$$(TOP)/$$(INPLACE_BIN)"'        >> $$@
	echo 'topdir="$$(TOP)/$$(INPLACE_TOPDIR)"'     >> $$@
	echo 'pgmgcc="$$(WhatGccIsCalled)"'            >> $$@
	$$($1_$2_SHELL_WRAPPER_EXTRA)
	$$($1_$2_INPLACE_SHELL_WRAPPER_EXTRA)
ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"
	cat $$($1_$2_SHELL_WRAPPER_NAME)               >> $$@
else
	echo 'exec "$executablename" $$$${1+"$$$$@"}'  >> $$@
endif
	$$(EXECUTABLE_FILE)                               $$@

endif

ifeq "$$($1_$2_WANT_INSTALLED_WRAPPER)" "YES"

ifeq "$$($1_$2_INSTALL_SHELL_WRAPPER_NAME)" ""
$1_$2_INSTALL_SHELL_WRAPPER_NAME = $$($1_$2_PROG)
endif

# Install the binary in $(libexecdir), and install a shell wrapper in $(bindir)
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

$(call profEnd, shell-wrapper($1,$2))
endef
