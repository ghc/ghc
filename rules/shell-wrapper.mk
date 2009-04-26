define shell-wrapper
# $1 = dir
# $2 = distdir

ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"

ifeq "$(Windows)" "YES"

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

$$(INPLACE_BIN)/$$($1_$2_PROG): $$($1_$2_INPLACE)
	$$(RM) -f                                         $$@
	echo '#!$$(SHELL)'                             >> $$@
	echo 'executablename=$$(TOP)/$$<'  >> $$@
	echo 'datadir=$$(TOP)/$$(INPLACE_LIB)' >> $$@
	echo 'bindir=$$(TOP)/$$(INPLACE_BIN)'  >> $$@
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
install_$1_$2_wrapper: WRAPPER=$$(DESTDIR)$$(bindir)/$$($1_$2_INSTALL_SHELL_WRAPPER_NAME)
install_$1_$2_wrapper:
	$$(MKDIRHIER) $$(DESTDIR)$$(bindir)
	$$(RM) -f                                      $$(WRAPPER)
	echo '#!$$(SHELL)'                          >> $$(WRAPPER)
	echo 'executablename=$$(libexecdir)/$$($1_$2_PROG)' >> $$(WRAPPER)
	echo 'datadir=$$(datadir)'                  >> $$(WRAPPER)
	echo 'bindir=$$(bindir)'                    >> $$(WRAPPER)
	cat $$($1_$2_SHELL_WRAPPER_NAME)            >> $$(WRAPPER)
	$$(EXECUTABLE_FILE)                            $$(WRAPPER)

endif # $1_$2_INSTALL_SHELL_WRAPPER

endif

endif # $1_$2_SHELL_WRAPPER

endef
