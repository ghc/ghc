#-----------------------------------------------------------------------------
# $Id: cdepend.mk,v 1.2 1996/11/21 16:50:30 simonm Exp $

# Useful variables:

# 	C_DEP_SRCS = 	C sources for make depend

#-----------------------------------------------------------------------------

depend	:: $(C_DEP_SRCS)
	@$(RM) .depend
	@touch .depend
	$(MKDEPENDC) $(MKDEPENDCFLAGS) -- $(CFLAGS) -- $(C_DEP_SRCS)

