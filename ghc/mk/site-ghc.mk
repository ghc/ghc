#-----------------------------------------------------------------------------
# $Id: site-ghc.mk,v 1.4 1996/11/25 14:53:12 simonm Exp $

# GHC_BUILD_FLAG_x
#	these are alternative flag names that can be given
# 	to the driver to indicate build x.

# GHC_BUILD_OPTS_x
#	these are lists of flags to be added when the driver
#	receives a $(GHC_BUILD_FLAG_x) flag.  Only valid for
#	user build ways.

# ================================================================
# BUILDS stuff: main sequential ones

# (these aren't used --simonm)

#GHC_BUILD_FLAG_normal =
#GHC_BUILD_FLAG_p      =
#GHC_BUILD_FLAG_t      =
#GHC_BUILD_FLAG_u      =

# === builds: concurrent and parallel ============================

#GHC_BUILD_FLAG_mc =
#GHC_BUILD_FLAG_mr =
#GHC_BUILD_FLAG_mt =
#GHC_BUILD_FLAG_mp =
#GHC_BUILD_FLAG_mg =

# === builds: non-std garbage collectors ==========================

GHC_BUILD_FLAG_2s = -gc-2s
GHC_BUILD_FLAG_1s = -gc-1s
GHC_BUILD_FLAG_du = -gc-du

# === builds: "user ways" =======================================

GHC_BUILD_FLAG_a = -build-a-not-defined
GHC_BUILD_OPTS_a =
		  
GHC_BUILD_FLAG_b = -build-b-not-defined
GHC_BUILD_OPTS_b =
		  
GHC_BUILD_FLAG_c = -build-c-not-defined
GHC_BUILD_OPTS_c =
		  
GHC_BUILD_FLAG_d = -build-d-not-defined
GHC_BUILD_OPTS_d =
		  
GHC_BUILD_FLAG_e = -build-e-not-defined
GHC_BUILD_OPTS_e =
		  
GHC_BUILD_FLAG_f = -build-f-not-defined
GHC_BUILD_OPTS_f =
		  
GHC_BUILD_FLAG_g = -build-g-not-defined
GHC_BUILD_OPTS_g =
		  
GHC_BUILD_FLAG_h = -build-h-not-defined
GHC_BUILD_OPTS_h =
		  
GHC_BUILD_FLAG_i = -build-i-not-defined
GHC_BUILD_OPTS_i =
		  
GHC_BUILD_FLAG_j = -build-j-not-defined
GHC_BUILD_OPTS_j =
		  
GHC_BUILD_FLAG_k = -build-k-not-defined
GHC_BUILD_OPTS_k =
		  
GHC_BUILD_FLAG_l = -build-l-not-defined
GHC_BUILD_OPTS_l =
		  
GHC_BUILD_FLAG_m = -build-m-not-defined
GHC_BUILD_OPTS_m =
		  
GHC_BUILD_FLAG_n = -build-n-not-defined
GHC_BUILD_OPTS_n =
		  
GHC_BUILD_FLAG_o = -build-o-not-defined
GHC_BUILD_OPTS_o =
		  
GHC_BUILD_FLAG_A = -build-A-not-defined
GHC_BUILD_OPTS_A =
		  
GHC_BUILD_FLAG_B = -build-B-not-defined
GHC_BUILD_OPTS_B =

# ======= END OF BUILD INFO ====================================

# Temp until we reliable bootstrap

Ghc2_0 = NO
