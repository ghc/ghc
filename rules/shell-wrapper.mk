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

ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"

ifeq "$$(Windows)" "YES"

ifeq "$$($1_$2_INSTALL_SHELL_WRAPPER)" "YES"
# Just install the binary on Windows
$1_$2_INSTALL = YES
endif

else

ifeq "$$($1_$2_SHELL_WRAPPER_NAME)" ""
$1_$2_SHELL_WRAPPER_NAME = $1/$$($1_$2_PROG).wrapper
endif

ifneq "$$($1_$2_INSTALL_INPLACE)" "NO"
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
	cat $$($1_$2_SHELL_WRAPPER_NAME)               >> $$@
	$$(EXECUTABLE_FILE)                               $$@
endif

ifeq "$$($1_$2_INSTALL_SHELL_WRAPPER)" "YES"

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
	echo 'exedir="$$(ghclibexecdir)"'                        >> "$$(WRAPPER)"
	echo 'exeprog="$$($1_$2_PROG)"'                          >> "$$(WRAPPER)"
	echo 'executablename="$$$$exedir/$$$$exeprog"'           >> "$$(WRAPPER)"
	echo 'datadir="$$(datadir)"'                             >> "$$(WRAPPER)"
	echo 'bindir="$$(bindir)"'                               >> "$$(WRAPPER)"
	echo 'topdir="$$(topdir)"'                               >> "$$(WRAPPER)"
	$$($1_$2_SHELL_WRAPPER_EXTRA)
	$$($1_$2_INSTALL_SHELL_WRAPPER_EXTRA)
	cat $$($1_$2_SHELL_WRAPPER_NAME)                         >> "$$(WRAPPER)"
	$$(EXECUTABLE_FILE)                                         "$$(WRAPPER)"

endif # $1_$2_INSTALL_SHELL_WRAPPER

endif

endif # $1_$2_SHELL_WRAPPER

$(call profEnd, shell-wrapper($1,$2))
endef
