# hand-hacked Makefile to boot the "make world" process
#---------------------------------------------------------------------

# Platform-specific configuration stuff was read from: sun.cf

SHELL = /bin/sh
PATHSEP = /

#******** NB: TOP setting
TOP = ..
CURRENT_DIR = ./ghc

RM=rm -f
MV=mv -f
BOOTSTRAPCFLAGS =

# Project identification -- name, version, and stuff

PROJECTNAME      = Booting the Make World System
PROJECTVERSION   = none
PROJECTLABEL     = generic
SETUPLABEL       = std

PROJECTCONFIGDIR =

###########################################################################
# Configuration stuff (jmake, its friends and templates)

JMKMF         = jmkmf_used_only_with_installed_utils
JRESTOREDEPS  = $(JMAKESRC)/jrestoredeps
JMAKE         = $(JMAKESRC)/jmake
JMAKE_DEFINES =

JMAKESRC = $(TOP)/mkworld

JMAKE_CMD = $(NEWTOP)$(JMAKE) -I$(NEWTOP)$(JMAKESRC) $(BOOTSTRAPCFLAGS) -DTopDir=$(TOP) -DCurDir=$(CURRENT_DIR) $(BOOT_DEFINES)

###########################################################################

Makefile:: $(JMAKE)

$(JMAKE):
	@(cd $(JMAKESRC) && if [ -f Makefile ]; then \
	echo "checking $@ in $(JMAKESRC) first..."; $(MAKE) all; else \
	echo "bootstrapping $@ from Makefile.BOOT in $(JMAKESRC) first..."; \
	$(MAKE) -f Makefile.BOOT BOOTSTRAPCFLAGS=$(BOOTSTRAPCFLAGS); fi; \
	echo "okay, continuing in $(CURRENT_DIR)")

Makefile::
	-@if [ -f Makefile ]; then \
	echo "	$(RM) Makefile.bak; $(MV) Makefile Makefile.bak"; \
	$(RM) Makefile.bak; $(MV) Makefile Makefile.bak; \
	else exit 0; fi
	$(JMAKE_CMD)
	$(JRESTOREDEPS)
	@if cmp -s Makefile Makefile.bak; then $(RM) Makefile.bak ; else exit 0 ; fi
	@chmod 444 Makefile
	@echo ==== The new Makefile is for\: ====
	@$(MAKE) whoami
