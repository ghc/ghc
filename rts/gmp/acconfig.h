/*
Copyright (C) 2000 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
*/

@TOP@

/* Define if a limb is long long. */
#undef _LONG_LONG_LIMB

/* Define if we have native implementation of function. */
#undef HAVE_NATIVE_
#undef HAVE_NATIVE_mpn_add                 
#undef HAVE_NATIVE_mpn_add_1               
#undef HAVE_NATIVE_mpn_add_n               
#undef HAVE_NATIVE_mpn_add_nc              
#undef HAVE_NATIVE_mpn_addmul_1            
#undef HAVE_NATIVE_mpn_addmul_1c
#undef HAVE_NATIVE_mpn_addsub_n            
#undef HAVE_NATIVE_mpn_addsub_nc
#undef HAVE_NATIVE_mpn_and_n               
#undef HAVE_NATIVE_mpn_andn_n              
#undef HAVE_NATIVE_mpn_bdivmod             
#undef HAVE_NATIVE_mpn_cmp                 
#undef HAVE_NATIVE_mpn_com_n               
#undef HAVE_NATIVE_mpn_copyd               
#undef HAVE_NATIVE_mpn_copyi               
#undef HAVE_NATIVE_mpn_divexact_by3c
#undef HAVE_NATIVE_mpn_divrem              
#undef HAVE_NATIVE_mpn_divrem_1            
#undef HAVE_NATIVE_mpn_divrem_1c
#undef HAVE_NATIVE_mpn_divrem_2            
#undef HAVE_NATIVE_mpn_divrem_newton       
#undef HAVE_NATIVE_mpn_divrem_classic      
#undef HAVE_NATIVE_mpn_dump                
#undef HAVE_NATIVE_mpn_gcd                 
#undef HAVE_NATIVE_mpn_gcd_1               
#undef HAVE_NATIVE_mpn_gcdext              
#undef HAVE_NATIVE_mpn_get_str             
#undef HAVE_NATIVE_mpn_hamdist             
#undef HAVE_NATIVE_mpn_invert_limb
#undef HAVE_NATIVE_mpn_ior_n               
#undef HAVE_NATIVE_mpn_iorn_n              
#undef HAVE_NATIVE_mpn_lshift              
#undef HAVE_NATIVE_mpn_mod_1               
#undef HAVE_NATIVE_mpn_mod_1c
#undef HAVE_NATIVE_mpn_mul                 
#undef HAVE_NATIVE_mpn_mul_1               
#undef HAVE_NATIVE_mpn_mul_1c
#undef HAVE_NATIVE_mpn_mul_basecase        
#undef HAVE_NATIVE_mpn_mul_n               
#undef HAVE_NATIVE_mpn_nand_n              
#undef HAVE_NATIVE_mpn_nior_n              
#undef HAVE_NATIVE_mpn_perfect_square_p    
#undef HAVE_NATIVE_mpn_popcount            
#undef HAVE_NATIVE_mpn_preinv_mod_1        
#undef HAVE_NATIVE_mpn_random2             
#undef HAVE_NATIVE_mpn_random              
#undef HAVE_NATIVE_mpn_rawrandom           
#undef HAVE_NATIVE_mpn_rshift              
#undef HAVE_NATIVE_mpn_scan0               
#undef HAVE_NATIVE_mpn_scan1               
#undef HAVE_NATIVE_mpn_set_str             
#undef HAVE_NATIVE_mpn_sqrtrem             
#undef HAVE_NATIVE_mpn_sqr_basecase        
#undef HAVE_NATIVE_mpn_sub                 
#undef HAVE_NATIVE_mpn_sub_1               
#undef HAVE_NATIVE_mpn_sub_n               
#undef HAVE_NATIVE_mpn_sub_nc              
#undef HAVE_NATIVE_mpn_submul_1            
#undef HAVE_NATIVE_mpn_submul_1c
#undef HAVE_NATIVE_mpn_udiv_w_sdiv         
#undef HAVE_NATIVE_mpn_umul_ppmm
#undef HAVE_NATIVE_mpn_udiv_qrnnd
#undef HAVE_NATIVE_mpn_xor_n               
#undef HAVE_NATIVE_mpn_xnor_n              
