#-----------------------------------------------------------------------------
# $Id: hsdepend.mk,v 1.2 1996/11/21 16:50:33 simonm Exp $

# Useful variables:

# 	HS_DEP_SRCS = 	C sources for make depend

#-----------------------------------------------------------------------------

depend	:: $(HS_DEP_SRCS)
	@$(RM) .depend
	@touch .depend
	$(MKDEPENDHS) $(MKDEPENDHSFLAGS) -- $(HCFLAGS) -- $(HS_DEP_SRCS)

