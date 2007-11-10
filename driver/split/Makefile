#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2000/11/03 16:54:52 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

PROG = ghc-split
all:: $(PROG).prl
	$(RM) -f $(PROG)
	echo '#!$(PERL)'                                >> $(PROG)
	echo '$$TARGETPLATFORM  = "$(TARGETPLATFORM)";' >> $(PROG)
	cat $<                                          >> $(PROG)
	$(EXECUTABLE_FILE) $(PROG)

INSTALL_LIBEXEC_SCRIPTS += $(PROG)
CLEAN_FILES += $(PROG) $(PROG).prl

binary-dist:
	$(INSTALL_DIR)             $(BIN_DIST_DIR)/driver/split
	$(INSTALL_DATA)   Makefile $(BIN_DIST_DIR)/driver/split/
	$(INSTALL_SCRIPT) $(PROG)  $(BIN_DIST_DIR)/driver/split/

include $(TOP)/mk/target.mk
