#-----------------------------------------------------------------------------
# $Id: site-ghc.mk,v 1.3 1996/11/25 14:45:22 simonm Exp $

# GHC_BUILD_FLAG_x
#	these are alternative flag names that can be given
# 	to the driver to indicate build x.

# GHC_BUILD_OPTS_x
#	these are lists of flags to be added when the driver
#	receives a $(GHC_BUILD_FLAG_x) flag.  Only valid for
#	user build ways.

# ================================================================
# BUILDS stuff: main sequential ones

GHC_BUILD_FLAG_normal =
GHC_BUILD_FLAG_p      =
GHC_BUILD_FLAG_t      =
GHC_BUILD_FLAG_u      =

# === builds: concurrent and parallel ============================

GHC_BUILD_FLAG_mc =
GHC_BUILD_FLAG_mr =
GHC_BUILD_FLAG_mt =
GHC_BUILD_FLAG_mp =
GHC_BUILD_FLAG_mg =

# === builds: non-std garbage collectors ==========================

GHC_BUILD_FLAG_2s = -gc-2s
GHC_BUILD_FLAG_1s = -gc-1s
GHC_BUILD_FLAG_du = -gc-du

# === builds: "user ways" =======================================

GHC_BUILD_FLAG_a =
GHC_BUILD_OPTS_a =
		  
GHC_BUILD_FLAG_b =
GHC_BUILD_OPTS_b =
		  
GHC_BUILD_FLAG_c =
GHC_BUILD_OPTS_c =
		  
GHC_BUILD_FLAG_d =
GHC_BUILD_OPTS_d =
		  
GHC_BUILD_FLAG_e =
GHC_BUILD_OPTS_e =
		  
GHC_BUILD_FLAG_f =
GHC_BUILD_OPTS_f =
		  
GHC_BUILD_FLAG_g =
GHC_BUILD_OPTS_g =
		  
GHC_BUILD_FLAG_h =
GHC_BUILD_OPTS_h =
		  
GHC_BUILD_FLAG_i =
GHC_BUILD_OPTS_i =
		  
GHC_BUILD_FLAG_j =
GHC_BUILD_OPTS_j =
		  
GHC_BUILD_FLAG_k =
GHC_BUILD_OPTS_k =
		  
GHC_BUILD_FLAG_l =
GHC_BUILD_OPTS_l =
		  
GHC_BUILD_FLAG_m =
GHC_BUILD_OPTS_m =
		  
GHC_BUILD_FLAG_n =
GHC_BUILD_OPTS_n =
		  
GHC_BUILD_FLAG_o =
GHC_BUILD_OPTS_o =
		  
GHC_BUILD_FLAG_A =
GHC_BUILD_OPTS_A =
		  
GHC_BUILD_FLAG_B =
GHC_BUILD_OPTS_B =

# ======= END OF BUILD INFO ====================================

# Temp until we reliable bootstrap

Ghc2_0 = NO
