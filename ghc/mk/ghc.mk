# -----------------------------------------------------------------------------
# $Id: ghc.mk,v 1.2 1996/11/21 16:48:00 simonm Exp $

# include the generic build stuff first...

include $(TOP)/mk/gen.mk

# then the specific GHC stuff, so we can override defaults if
# necessary.

include $(TOP)/ghc/mk/buildinfo.mk
include $(TOP)/ghc/mk/site-ghc.mk
include $(TOP)/ghc/mk/ghcconfig.mk
include $(TOP)/ghc/mk/suffixes-ghc.mk
