#-----------------------------------------------------------------------------
# $Id: ghc-opts.mk,v 1.2 1996/11/21 16:47:59 simonm Exp $

ifdef DoingRTS
GCap = -optc-DGCap
GC2s = -optc-DGC2s
GC1s = -optc-DG1s
endif

GHC_OPTS_norm=-O $(GCap)
GHC_OPTS_p =-O -prof -GPrelude $(GCap) -hisuf p_hi -hisuf-prelude p_hi
GHC_OPTS_t =-O -ticky -optc-DDEBUG $(GCap) -hisuf t_hi -hisuf-prelude t_hi
GHC_OPTS_u =-O -unregisterised ???? -ticky $(GCap) -hisuf u_hi -hisuf-prelude u_hi
GHC_OPTS_mc=-O -concurrent $(GCap) -hisuf mc_hi -hisuf-prelude mc_hi
GHC_OPTS_mr=-O -concurrent -prof -GPrelude $(GCap) -hisuf mr_hi -hisuf-prelude mr_hi
GHC_OPTS_mt=-O -concurrent -ticky -optc-DDEBUG $(GCap) -hisuf mt_hi -hisuf-prelude mt_hi
GHC_OPTS_mp=-O -parallel $(GCap) -hisuf mp_hi -hisuf-prelude mp_hi
GHC_OPTS_mg=-O -gransim $(GCap) -hisuf mg_hi -hisuf-prelude mg_hi

GHC_OPTS_2s=-O -gc-2s $(GC2s) -hisuf 2s_hi -hisuf-prelude 2s_hi
GHC_OPTS_1s=-O -gc-1s $(GC1s) -hisuf 1s_hi -hisuf-prelude 1s_hi
GHC_OPTS_du=-O -gc-du $(GCdu) -hisuf du_hi -hisuf-prelude du_hi

GHC_OPTS_a =-user-setup-a $(GCap) -hisuf a_hi -hisuf-prelude a_hi
GHC_OPTS_b =-user-setup-b $(GCap) -hisuf b_hi -hisuf-prelude b_hi
GHC_OPTS_c =-user-setup-c $(GCap) -hisuf c_hi -hisuf-prelude c_hi
GHC_OPTS_d =-user-setup-d $(GCap) -hisuf d_hi -hisuf-prelude d_hi
GHC_OPTS_e =-user-setup-e $(GCap) -hisuf e_hi -hisuf-prelude e_hi
GHC_OPTS_f =-user-setup-f $(GCap) -hisuf f_hi -hisuf-prelude f_hi
GHC_OPTS_g =-user-setup-g $(GCap) -hisuf g_hi -hisuf-prelude g_hi
GHC_OPTS_h =-user-setup-h $(GCap) -hisuf h_hi -hisuf-prelude h_hi
GHC_OPTS_i =-user-setup-i $(GCap) -hisuf i_hi -hisuf-prelude i_hi
GHC_OPTS_j =-user-setup-j $(GCap) -hisuf j_hi -hisuf-prelude j_hi
GHC_OPTS_k =-user-setup-k $(GCap) -hisuf k_hi -hisuf-prelude k_hi
GHC_OPTS_l =-user-setup-l $(GCap) -hisuf l_hi -hisuf-prelude l_hi
GHC_OPTS_m =-user-setup-m $(GCap) -hisuf m_hi -hisuf-prelude m_hi
GHC_OPTS_n =-user-setup-n $(GCap) -hisuf n_hi -hisuf-prelude n_hi
GHC_OPTS_o =-user-setup-o $(GCap) -hisuf o_hi -hisuf-prelude o_hi
GHC_OPTS_A =-user-setup-A $(GCap) -hisuf A_hi -hisuf-prelude A_hi
GHC_OPTS_B =-user-setup-B $(GCap) -hisuf B_hi -hisuf-prelude B_hi

# used in hslibs:

HC_OPTS_norm= $(GHC_OPTS_norm)
HC_OPTS_p = $(GHC_OPTS_p)
HC_OPTS_t = $(GHC_OPTS_t)
HC_OPTS_u = $(GHC_OPTS_u)
HC_OPTS_mc= $(GHC_OPTS_mc)
HC_OPTS_mr= $(GHC_OPTS_mr)
HC_OPTS_mt= $(GHC_OPTS_mt)
HC_OPTS_mp= $(GHC_OPTS_mp)
HC_OPTS_mg= $(GHC_OPTS_mg)
HC_OPTS_2s= $(GHC_OPTS_2s)
HC_OPTS_1s= $(GHC_OPTS_1s)
HC_OPTS_du= $(GHC_OPTS_du)
HC_OPTS_a = $(GHC_OPTS_a)
HC_OPTS_b = $(GHC_OPTS_b)
HC_OPTS_c = $(GHC_OPTS_c)
HC_OPTS_d = $(GHC_OPTS_d)
HC_OPTS_e = $(GHC_OPTS_e)
HC_OPTS_f = $(GHC_OPTS_f)
HC_OPTS_g = $(GHC_OPTS_g)
HC_OPTS_h = $(GHC_OPTS_h)
HC_OPTS_i = $(GHC_OPTS_i)
HC_OPTS_j = $(GHC_OPTS_j)
HC_OPTS_k = $(GHC_OPTS_k)
HC_OPTS_l = $(GHC_OPTS_l)
HC_OPTS_m = $(GHC_OPTS_m)
HC_OPTS_n = $(GHC_OPTS_n)
HC_OPTS_o = $(GHC_OPTS_o)
HC_OPTS_A = $(GHC_OPTS_A)
HC_OPTS_B = $(GHC_OPTS_B)

#-----------------------------------------------------------------------------
# Build up a list of the suffixes for which we're building

# this stuff is used by the rts and lib Makefiles.

WAY_SUFFIXES =

ifeq ($(Build_p), YES)
WAY_SUFFIXES += p
endif 

ifeq ($(Build_t), YES)
WAY_SUFFIXES += t
endif 

ifeq ($(Build_u), YES)
WAY_SUFFIXES += u
endif 

ifeq ($(Build_mc), YES)
WAY_SUFFIXES += mc
endif 

ifeq ($(Build_mr), YES)
WAY_SUFFIXES += mr
endif 

ifeq ($(Build_mt), YES)
WAY_SUFFIXES += mt
endif 

ifeq ($(Build_mp), YES)
WAY_SUFFIXES += mp
endif 

ifeq ($(Build_mg), YES)
WAY_SUFFIXES += mg
endif 

ifeq ($(Build_2s), YES)
WAY_SUFFIXES += 2s
endif 

ifeq ($(Build_1s), YES)
WAY_SUFFIXES += 1s
endif 

ifeq ($(Build_du), YES)
WAY_SUFFIXES += du
endif 

ifeq ($(Build_a), YES)
WAY_SUFFIXES += a
endif 

ifeq ($(Build_b), YES)
WAY_SUFFIXES += b
endif 

ifeq ($(Build_c), YES)
WAY_SUFFIXES += c
endif 

ifeq ($(Build_d), YES)
WAY_SUFFIXES += d
endif 

ifeq ($(Build_e), YES)
WAY_SUFFIXES += e
endif 

ifeq ($(Build_f), YES)
WAY_SUFFIXES += f
endif 

ifeq ($(Build_g), YES)
WAY_SUFFIXES += g
endif 

ifeq ($(Build_h), YES)
WAY_SUFFIXES += h
endif 

ifeq ($(Build_i), YES)
WAY_SUFFIXES += i
endif 

ifeq ($(Build_j), YES)
WAY_SUFFIXES += j
endif 

ifeq ($(Build_k), YES)
WAY_SUFFIXES += k
endif 

ifeq ($(Build_l), YES)
WAY_SUFFIXES += l
endif 

ifeq ($(Build_m), YES)
WAY_SUFFIXES += m
endif 

ifeq ($(Build_n), YES)
WAY_SUFFIXES += n
endif 

ifeq ($(Build_o), YES)
WAY_SUFFIXES += o
endif 

ifeq ($(Build_A), YES)
WAY_SUFFIXES += A 
endif 

ifeq ($(Build_B), YES)
WAY_SUFFIXES += B
endif 

