/* -----------------------------------------------------------------------------
 * $Id: HeapStackCheck.h,v 1.7 2001/07/06 14:11:38 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in HeapStackCheck.hc
 *
 * ---------------------------------------------------------------------------*/


EXTFUN(stg_gc_entertop);
EXTFUN(stg_gc_enter_1_hponly);
EXTFUN(stg_gc_enter_1);
EXTFUN(stg_gc_enter_2);
EXTFUN(stg_gc_enter_3);
EXTFUN(stg_gc_enter_4);
EXTFUN(stg_gc_enter_5);
EXTFUN(stg_gc_enter_6);
EXTFUN(stg_gc_enter_7);
EXTFUN(stg_gc_enter_8);
EXTFUN(stg_gc_seq_1);

EI_(stg_gc_noregs_ret_info);
EF_(stg_gc_noregs);

EI_(stg_gc_unpt_r1_ret_info);
EF_(stg_gc_unpt_r1);

EI_(stg_gc_unbx_r1_ret_info);
EF_(stg_gc_unbx_r1);

EI_(stg_gc_f1_ret_info);
EF_(stg_gc_f1);

EI_(stg_gc_d1_ret_info);
EF_(stg_gc_d1);

EI_(stg_gc_ut_1_0_ret_info);
EF_(stg_gc_ut_1_0);

EI_(stg_gc_ut_0_1_ret_info);
EF_(stg_gc_ut_0_1);

EXTFUN(stg_chk_0);
EXTFUN(stg_chk_1);
EXTFUN(stg_chk_1n);
EXTFUN(stg_chk_2);
EXTFUN(stg_chk_3);
EXTFUN(stg_chk_4);
EXTFUN(stg_chk_5);
EXTFUN(stg_chk_6);
EXTFUN(stg_chk_7);
EXTFUN(stg_chk_8);
EXTFUN(stg_gen_chk_ret);
EXTFUN(stg_gen_chk);
EXTFUN(stg_gen_hp);
EXTFUN(stg_gen_yield);
EXTFUN(stg_yield_noregs);
EXTFUN(stg_yield_to_interpreter);
EXTFUN(stg_gen_block);
EXTFUN(stg_block_noregs);
EXTFUN(stg_block_1);
EXTFUN(stg_block_takemvar);
EXTFUN(stg_block_putmvar);
