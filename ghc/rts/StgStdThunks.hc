/* -----------------------------------------------------------------------------
 * $Id: StgStdThunks.hc,v 1.22 2003/04/18 09:40:10 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Canned "Standard Form" Thunks
 *
 * ---------------------------------------------------------------------------*/

#include "Stg.h"
#include "Rts.h"
#include "StoragePriv.h"

/* -----------------------------------------------------------------------------
   The code for a thunk that simply extracts a field from a
   single-constructor datatype depends only on the offset of the field
   to be selected.

   Here we define some canned "selector" thunks that do just that; any
   selector thunk appearing in a program will refer to one of these
   instead of being compiled independently.

   The garbage collector spots selector thunks and reduces them if
   possible, in order to avoid space leaks resulting from lazy pattern
   matching.
   -------------------------------------------------------------------------- */

#define UPD_FRAME_SIZE (sizeofW(StgUpdateFrame)+sizeofW(StgHeader))
#define NOUPD_FRAME_SIZE (sizeofW(StgHeader))

#ifdef PROFILING
#define SAVE_CCCS(fs)  	CCS_HDR(Sp-fs)=CCCS
#define GET_SAVED_CCCS  RESTORE_CCCS(CCS_HDR(Sp))
#define ENTER_CCS(p)    ENTER_CCS_TCL(p)
#define RET_BITMAP    3
#define RET_FRAMESIZE 2
#else
#define SAVE_CCCS(fs)   /* empty */
#define GET_SAVED_CCCS  /* empty */
#define ENTER_CCS(p)    /* empty */
#define RET_BITMAP    0
#define RET_FRAMESIZE 0
#endif

#define SELECTOR_CODE_UPD(offset) \
  IF_(stg_sel_ret_##offset##_upd_ret);					\
  INFO_TABLE_RET(stg_sel_ret_##offset##_upd_info,stg_sel_ret_##offset##_upd_ret, MK_SMALL_BITMAP(RET_FRAMESIZE, RET_BITMAP), 0, 0, 0, RET_SMALL, static, EF_, 0, 0);	\
  EF_(stg_sel_ret_##offset##_upd_ret) {					\
    FB_									\
      R1.p=(P_)R1.cl->payload[offset];					\
      GET_SAVED_CCCS;							\
      Sp=Sp+sizeofW(StgHeader);						\
      ENTER();								\
    FE_									\
  }									\
									\
  EF_(stg_sel_##offset##_upd_entry);					\
  INFO_TABLE_SELECTOR(stg_sel_##offset##_upd_info, stg_sel_##offset##_upd_entry, offset,, EF_, "stg_sel" #offset "_upd_entry", "stg_sel" #offset "_upd_entry");\
  EF_(stg_sel_##offset##_upd_entry) {					\
    FB_									\
      TICK_ENT_DYN_THK();  /* is it static or dynamic?? */              \
      STK_CHK_NP(UPD_FRAME_SIZE,);					\
      UPD_BH_UPDATABLE(&stg_sel_##offset##_upd_info);			\
      LDV_ENTER(R1.cl);							\
      PUSH_UPD_FRAME(R1.p,0);						\
      ENTER_CCS(R1.p);							\
      SAVE_CCCS(UPD_FRAME_SIZE);					\
      Sp[-UPD_FRAME_SIZE]=(W_)&stg_sel_ret_##offset##_upd_info;		\
      R1.p = (P_)R1.cl->payload[0];					\
      Sp=Sp-UPD_FRAME_SIZE;						\
      ENTER();								\
    FE_									\
  }

SELECTOR_CODE_UPD(0);
SELECTOR_CODE_UPD(1);
SELECTOR_CODE_UPD(2);
SELECTOR_CODE_UPD(3);
SELECTOR_CODE_UPD(4);
SELECTOR_CODE_UPD(5);
SELECTOR_CODE_UPD(6);
SELECTOR_CODE_UPD(7);
SELECTOR_CODE_UPD(8);
SELECTOR_CODE_UPD(9);
SELECTOR_CODE_UPD(10);
SELECTOR_CODE_UPD(11);
SELECTOR_CODE_UPD(12);
SELECTOR_CODE_UPD(13);
SELECTOR_CODE_UPD(14);
SELECTOR_CODE_UPD(15);

#define SELECTOR_CODE_NOUPD(offset) \
  IF_(stg_sel_ret_##offset##_noupd_ret); \
  INFO_TABLE_RET(stg_sel_ret_##offset##_noupd_info, stg_sel_ret_##offset##_noupd_ret, MK_SMALL_BITMAP(RET_FRAMESIZE, RET_BITMAP), 0, 0, 0, RET_SMALL, static, EF_, 0, 0);	\
  IF_(stg_sel_ret_##offset##_noupd_ret) {					\
    FB_									\
      R1.p=(P_)R1.cl->payload[offset];					\
      GET_SAVED_CCCS;							\
      Sp=Sp+sizeofW(StgHeader);						\
      JMP_(ENTRY_CODE(*R1.p));						\
    FE_									\
  }									\
									\
  EF_(stg_sel_##offset##_noupd_entry);					\
  INFO_TABLE_SELECTOR(stg_sel_##offset##_noupd_info, stg_sel_##offset##_noupd_entry, offset,, EF_, "stg_sel" #offset "_noupd_entry", "stg_sel" #offset "_noupd_entry");\
  EF_(stg_sel_##offset##_noupd_entry) {					\
    FB_									\
      TICK_ENT_DYN_THK();  /* is it static or dynamic?? */              \
      STK_CHK_NP(NOUPD_FRAME_SIZE,)					\
      UPD_BH_SINGLE_ENTRY(&stg_sel_##offset##_noupd_info);		\
      LDV_ENTER(R1.cl);							\
      TICK_UPDF_OMITTED();						\
      ENTER_CCS(R1.p);							\
      SAVE_CCCS(NOUPD_FRAME_SIZE);					\
      Sp[-NOUPD_FRAME_SIZE]=(W_)&stg_sel_ret_##offset##_noupd_info;	\
      R1.p = (P_)R1.cl->payload[0];					\
      Sp=Sp-NOUPD_FRAME_SIZE;						\
      JMP_(ENTRY_CODE(*R1.p));						\
    FE_									\
  }

SELECTOR_CODE_NOUPD(0);
SELECTOR_CODE_NOUPD(1);
SELECTOR_CODE_NOUPD(2);
SELECTOR_CODE_NOUPD(3);
SELECTOR_CODE_NOUPD(4);
SELECTOR_CODE_NOUPD(5);
SELECTOR_CODE_NOUPD(6);
SELECTOR_CODE_NOUPD(7);
SELECTOR_CODE_NOUPD(8);
SELECTOR_CODE_NOUPD(9);
SELECTOR_CODE_NOUPD(10);
SELECTOR_CODE_NOUPD(11);
SELECTOR_CODE_NOUPD(12);
SELECTOR_CODE_NOUPD(13);
SELECTOR_CODE_NOUPD(14);
SELECTOR_CODE_NOUPD(15);

/* -----------------------------------------------------------------------------
   Apply thunks

   An apply thunk is a thunk of the form
	
		let z = [x1...xn] \u x1...xn
		in ...

   We pre-compile some of these because the code is always the same.

   These have to be independent of the update frame size, so the code
   works when profiling etc.
   -------------------------------------------------------------------------- */

FN_(stg_ap_1_upd_entry);
FN_(stg_ap_2_upd_entry);
FN_(stg_ap_3_upd_entry);
FN_(stg_ap_4_upd_entry);
FN_(stg_ap_5_upd_entry);
FN_(stg_ap_6_upd_entry);
FN_(stg_ap_7_upd_entry);
FN_(stg_ap_8_upd_entry);

#define UF_SIZE (sizeofW(StgUpdateFrame))

/* stg_ap_1_upd_info is a bit redundant, but there appears to be a bug
 * in the compiler that means stg_ap_1 is generated occasionally (ToDo)
 */

INFO_TABLE_THUNK(stg_ap_1_upd_info,stg_ap_1_upd_entry,1,1,0,0,0,THUNK_1_0,,EF_,"stg_ap_1_upd_info","stg_ap_1_upd_info");
FN_(stg_ap_1_upd_entry) {
  FB_
  TICK_ENT_DYN_THK();  /* is it static or dynamic?? */
  STK_CHK_NP(sizeofW(StgUpdateFrame)+1,);
  UPD_BH_UPDATABLE(&stg_ap_1_upd_info);
  LDV_ENTER(R1.cl);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp -= sizeofW(StgUpdateFrame);
  Sp--; // for stg_ap_0_ret
  JMP_(stg_ap_0_ret);
  FE_
}

INFO_TABLE_THUNK(stg_ap_2_upd_info,stg_ap_2_upd_entry,2,0,0,0,0,THUNK_2_0,,EF_,"stg_ap_2_upd_info","stg_ap_2_upd_info");
FN_(stg_ap_2_upd_entry) {
  FB_
  TICK_ENT_DYN_THK();  /* is it static or dynamic?? */
  STK_CHK_NP(sizeofW(StgUpdateFrame)+2,);
  UPD_BH_UPDATABLE(&stg_ap_2_upd_info);
  LDV_ENTER(R1.cl);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp -= sizeofW(StgUpdateFrame)+1;
  Sp--; // for stg_ap_1_ret
  JMP_(stg_ap_p_ret);
  FE_
}

INFO_TABLE_THUNK(stg_ap_3_upd_info,stg_ap_3_upd_entry,3,0,0,0,0,THUNK,,EF_,"stg_ap_3_upd_info","stg_ap_3_upd_info");
FN_(stg_ap_3_upd_entry) {
  FB_
  TICK_ENT_DYN_THK();  /* is it static or dynamic?? */
  STK_CHK_NP(sizeofW(StgUpdateFrame)+3,);
  UPD_BH_UPDATABLE(&stg_ap_3_upd_info);
  LDV_ENTER(R1.cl);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp -= sizeofW(StgUpdateFrame)+2;
  Sp--; // for stg_ap_pp_ret
  JMP_(stg_ap_pp_ret);
  FE_
}

INFO_TABLE_THUNK(stg_ap_4_upd_info,stg_ap_4_upd_entry,4,0,0,0,0,THUNK,,EF_,"stg_ap_4_upd_info","stg_ap_4_upd_info");
FN_(stg_ap_4_upd_entry) {
  FB_
  TICK_ENT_DYN_THK();  /* is it static or dynamic?? */
  STK_CHK_NP(sizeofW(StgUpdateFrame)+4,);
  UPD_BH_UPDATABLE(&stg_ap_4_upd_info);
  LDV_ENTER(R1.cl);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[3]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-3]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+3);
  Sp--; // for stg_ap_ppp_ret
  JMP_(stg_ap_ppp_ret);
  FE_
}

INFO_TABLE_THUNK(stg_ap_5_upd_info,stg_ap_5_upd_entry,5,0,0,0,0,THUNK,,EF_,"stg_ap_5_upd_info","stg_ap_5_upd_info");
FN_(stg_ap_5_upd_entry) {
  FB_
  TICK_ENT_DYN_THK();  /* is it static or dynamic?? */
  STK_CHK_NP(sizeofW(StgUpdateFrame)+5,);
  UPD_BH_UPDATABLE(&stg_ap_5_upd_info);
  LDV_ENTER(R1.cl);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[4]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[3]);
  Sp[-UF_SIZE-3]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-4]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+4);
  Sp--; // for stg_ap_pppp_ret
  JMP_(stg_ap_pppp_ret);
  FE_
}

INFO_TABLE_THUNK(stg_ap_6_upd_info,stg_ap_6_upd_entry,6,0,0,0,0,THUNK,,EF_,"stg_ap_6_upd_info","stg_ap_6_upd_info");
FN_(stg_ap_6_upd_entry) {
  FB_
  TICK_ENT_DYN_THK();  /* is it static or dynamic?? */
  STK_CHK_NP(sizeofW(StgUpdateFrame)+6,);
  UPD_BH_UPDATABLE(&stg_ap_6_upd_info);
  LDV_ENTER(R1.cl);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[5]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[4]);
  Sp[-UF_SIZE-3]=(W_)(R1.cl->payload[3]);
  Sp[-UF_SIZE-4]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-5]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+5);
  Sp--; // for stg_ap_ppppp_ret
  JMP_(stg_ap_ppppp_ret);
  FE_
}

INFO_TABLE_THUNK(stg_ap_7_upd_info,stg_ap_7_upd_entry,7,0,0,0,0,THUNK,,EF_,"stg_ap_7_upd_info","stg_ap_7_upd_info");
FN_(stg_ap_7_upd_entry) {
  FB_
  TICK_ENT_DYN_THK();  /* is it static or dynamic?? */
  STK_CHK_NP(sizeofW(StgUpdateFrame)+7,);
  UPD_BH_UPDATABLE(&stg_ap_7_upd_info);
  LDV_ENTER(R1.cl);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[6]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[5]);
  Sp[-UF_SIZE-3]=(W_)(R1.cl->payload[4]);
  Sp[-UF_SIZE-4]=(W_)(R1.cl->payload[3]);
  Sp[-UF_SIZE-5]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-6]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+6);
  Sp--; // for stg_ap_pppppp_ret
  JMP_(stg_ap_pppppp_ret);
  FE_
}

INFO_TABLE_THUNK(stg_ap_8_upd_info,stg_ap_8_upd_entry,8,0,0,0,0,THUNK,,EF_,"stg_ap_8_upd_info","stg_ap_8_upd_info");
FN_(stg_ap_8_upd_entry) {
  FB_
  TICK_ENT_DYN_THK();  /* is it static or dynamic?? */
  STK_CHK_NP(sizeofW(StgUpdateFrame)+8,);
  UPD_BH_UPDATABLE(&stg_ap_8_upd_info);
  LDV_ENTER(R1.cl);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[7]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[6]);
  Sp[-UF_SIZE-3]=(W_)(R1.cl->payload[5]);
  Sp[-UF_SIZE-4]=(W_)(R1.cl->payload[4]);
  Sp[-UF_SIZE-5]=(W_)(R1.cl->payload[3]);
  Sp[-UF_SIZE-6]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-7]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+7);
  Sp--; // for stg_ap_ppppppp_ret
  JMP_(stg_ap_ppppppp_ret);
  FE_
}
