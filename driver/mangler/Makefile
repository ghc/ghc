#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2001/03/23 16:36:21 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

PROG = ghc-asm
all:: $(PROG).prl
	$(RM) -f $(PROG)
	echo '#!$(PERL)'                                >> $(PROG)
	echo '$$TARGETPLATFORM  = "$(TARGETPLATFORM)";' >> $(PROG)
	cat $<                                          >> $(PROG)
	$(EXECUTABLE_FILE) $(PROG)

INSTALL_LIBEXEC_SCRIPTS += $(PROG)
CLEAN_FILES += $(PROG) $(PROG).prl

# needed for bootstrapping with HC files
ifeq "$(BootingFromHc)" "YES"
boot :: all
endif

binary-dist:
	$(INSTALL_DIR)             $(BIN_DIST_DIR)/driver/mangler
	$(INSTALL_DATA)   Makefile $(BIN_DIST_DIR)/driver/mangler/
	$(INSTALL_SCRIPT) $(PROG)  $(BIN_DIST_DIR)/driver/mangler/

include $(TOP)/mk/target.mk
