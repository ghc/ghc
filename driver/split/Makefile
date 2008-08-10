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

BINDIST_EXTRAS += $(PROG)
include $(TOP)/mk/bindist.mk

include $(TOP)/mk/target.mk
