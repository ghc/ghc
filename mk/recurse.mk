
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
ifeq "$(way)" ""
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

$(ALL_TARGET) docs runtests $(BOOT_TARGET) TAGS clean distclean mostlyclean maintainer-clean $(INSTALL_TARGET) $(INSTALL_DOCS_TARGET) html chm HxS ps dvi txt::
	@echo "------------------------------------------------------------------------"
	@echo "== Recursively making \`$@' in $(SUBDIRS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"
# Don't rely on -e working, instead we check exit return codes from sub-makes.
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	if [ $$x_on_err -eq 0 ]; \
	    then echo "Won't exit on error due to MFLAGS: ${MFLAGS}"; \
	fi; \
	for i in $(SUBDIRS); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "== $(MAKE) $@ $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(MFLAGS) $@ CLEAN_ALL_STAGES=YES; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ]; \
	      then echo "Finished making $@ in $$i": $$?; \
	      else echo "Failed making $@ in $$i": $$?; exit 1; \
	  fi; \
	done
	@echo "------------------------------------------------------------------------"
	@echo "== Finished making \`$@' in $(SUBDIRS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"

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

