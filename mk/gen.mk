#-----------------------------------------------------------------------------
# $Id: gen.mk,v 1.2 1996/11/21 16:50:32 simonm Exp $

# General include file for the top of a Makefile.

# So `all' is the default target...

all ::
	@:

include $(TOP)/mk/platform.mk
include $(TOP)/mk/utils.mk
include $(TOP)/mk/rules.mk
include $(TOP)/mk/install.mk

# fastmake omits the dependencies

ifndef FAST
-include .depend
endif

whoami::
	@echo $(PROJECTNAME), version $(PROJECTVERSION) $(PROJECTPATCHLEVEL)
	@echo project\: $(PROJECTLABEL)\; setup\: $(SETUPLABEL)
	@echo now building on a \`$(BUILDPLATFORM)\' host
	@echo hoping to run on a \`$(HOSTPLATFORM)\' host

clean::
	$(RM) $(FilesToClean) \#*

veryclean::
	$(RM) $(FilesToClean) $(ExtraFilesToBeVeryClean) \#*

tags::
	$(RM) TAGS; touch TAGS

depend::
