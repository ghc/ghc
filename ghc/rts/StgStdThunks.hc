/* -----------------------------------------------------------------------------
 * $Id: StgStdThunks.hc,v 1.10 1999/11/09 15:46:58 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Canned "Standard Form" Thunks
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "StoragePriv.h"
#include "HeapStackCheck.h"

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
#define RET_BITMAP 1
#else
#define SAVE_CCCS(fs)   /* empty */
#define GET_SAVED_CCCS  /* empty */
#define ENTER_CCS(p)    /* empty */
#define RET_BITMAP 0
#endif

#define SELECTOR_CODE_UPD(offset) \
  EF_(__sel_ret_##offset##_upd_ret);					\
  INFO_TABLE_SRT_BITMAP(__sel_ret_##offset##_upd_info,__sel_ret_##offset##_upd_ret, RET_BITMAP, 0, 0, 0, RET_SMALL, static, EF_, 0, 0);			\
  EF_(__sel_ret_##offset##_upd_ret) {					\
    FB_									\
      R1.p=(P_)R1.cl->payload[offset];					\
      GET_SAVED_CCCS;							\
      Sp=Sp+sizeofW(StgHeader);						\
      JMP_(ENTRY_CODE(*R1.p));						\
    FE_									\
  }									\
									\
  EF_(__sel_##offset##_upd_entry);					\
  INFO_TABLE_SELECTOR(__sel_##offset##_upd_info, __sel_##offset##_upd_entry, offset,, EF_, 0,0);\
  EF_(__sel_##offset##_upd_entry) {					\
    FB_									\
      STK_CHK_NP(UPD_FRAME_SIZE,1,);					\
      UPD_BH_UPDATABLE(&__sel_##offset##_upd_info);			\
      PUSH_UPD_FRAME(R1.p,0);						\
      ENTER_CCS(R1.p);							\
      SAVE_CCCS(UPD_FRAME_SIZE);					\
      Sp[-UPD_FRAME_SIZE]=(W_)&__sel_ret_##offset##_upd_info;		\
      R1.p = (P_)R1.cl->payload[0];					\
      Sp=Sp-UPD_FRAME_SIZE;						\
      JMP_(ENTRY_CODE(*R1.p));						\
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
  INFO_TABLE_SRT_BITMAP(__sel_ret_##offset##_noupd_info, __sel_ret_##offset##_noupd_ret, RET_BITMAP, 0, 0, 0, RET_SMALL, static, EF_, 0, 0);	\
  EF_(__sel_ret_##offset##_noupd_ret) {					\
    FB_									\
      R1.p=(P_)R1.cl->payload[offset];					\
      GET_SAVED_CCCS;							\
      Sp=Sp+sizeofW(StgHeader);						\
      JMP_(ENTRY_CODE(*R1.p));						\
    FE_									\
  }									\
									\
  EF_(__sel_##offset##_noupd_entry);					\
  INFO_TABLE_SELECTOR(__sel_##offset##_noupd_info, __sel_##offset##_noupd_entry, offset,, EF_, 0,0);\
  EF_(__sel_##offset##_noupd_entry) {					\
    FB_									\
      STK_CHK_NP(NOUPD_FRAME_SIZE,1,)					\
      UPD_BH_SINGLE_ENTRY(&__sel_##offset##_noupd_info);		\
      ENTER_CCS(R1.p);							\
      SAVE_CCCS(NOUPD_FRAME_SIZE);					\
      Sp[-NOUPD_FRAME_SIZE]=(W_)&__sel_ret_##offset##_noupd_info;	\
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

FN_(__ap_1_upd_entry);
FN_(__ap_2_upd_entry);
FN_(__ap_3_upd_entry);
FN_(__ap_4_upd_entry);
FN_(__ap_5_upd_entry);
FN_(__ap_6_upd_entry);
FN_(__ap_7_upd_entry);
FN_(__ap_8_upd_entry);

#define UF_SIZE (sizeofW(StgUpdateFrame))

/* __ap_1_upd_info is a bit redundant, but there appears to be a bug
 * in the compiler that means __ap_1 is generated occasionally (ToDo)
 */

INFO_TABLE_SRT(__ap_1_upd_info,__ap_1_upd_entry,1,0,0,0,0,THUNK,,EF_,0,0);
FN_(__ap_1_upd_entry) {
  FB_
  STK_CHK_NP(sizeofW(StgUpdateFrame),1,);
  UPD_BH_UPDATABLE(&__ap_1_upd_info);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - sizeofW(StgUpdateFrame);
  JMP_(ENTRY_CODE(*R1.p));
  FE_
}

INFO_TABLE_SRT(__ap_2_upd_info,__ap_2_upd_entry,2,0,0,0,0,THUNK,,EF_,0,0);
FN_(__ap_2_upd_entry) {
  FB_
  STK_CHK_NP(sizeofW(StgUpdateFrame)+1,1,);
  UPD_BH_UPDATABLE(&__ap_2_upd_info);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+1);
  JMP_(ENTRY_CODE(*R1.p));
  FE_
}

INFO_TABLE_SRT(__ap_3_upd_info,__ap_3_upd_entry,3,0,0,0,0,THUNK,,EF_,0,0);
FN_(__ap_3_upd_entry) {
  FB_
  STK_CHK_NP(sizeofW(StgUpdateFrame)+2,1,);
  UPD_BH_UPDATABLE(&__ap_3_upd_info);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+2);
  JMP_(ENTRY_CODE(*R1.p));
  FE_
}

INFO_TABLE_SRT(__ap_4_upd_info,__ap_4_upd_entry,4,0,0,0,0,THUNK,,EF_,0,0);
FN_(__ap_4_upd_entry) {
  FB_
  STK_CHK_NP(sizeofW(StgUpdateFrame)+3,1,);
  UPD_BH_UPDATABLE(&__ap_4_upd_info);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[3]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-3]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+3);
  JMP_(ENTRY_CODE(*R1.p));
  FE_
}

INFO_TABLE_SRT(__ap_5_upd_info,__ap_5_upd_entry,5,0,0,0,0,THUNK,,EF_,0,0);
FN_(__ap_5_upd_entry) {
  FB_
  STK_CHK_NP(sizeofW(StgUpdateFrame)+4,1,);
  UPD_BH_UPDATABLE(&__ap_5_upd_info);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[4]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[3]);
  Sp[-UF_SIZE-3]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-4]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+4);
  JMP_(ENTRY_CODE(*R1.p));
  FE_
}

INFO_TABLE_SRT(__ap_6_upd_info,__ap_6_upd_entry,6,0,0,0,0,THUNK,,EF_,0,0);
FN_(__ap_6_upd_entry) {
  FB_
  STK_CHK_NP(sizeofW(StgUpdateFrame)+5,1,);
  UPD_BH_UPDATABLE(&__ap_6_upd_info);
  ENTER_CCS(R1.p);
  PUSH_UPD_FRAME(R1.p,0);
  Sp[-UF_SIZE-1]=(W_)(R1.cl->payload[5]);
  Sp[-UF_SIZE-2]=(W_)(R1.cl->payload[4]);
  Sp[-UF_SIZE-3]=(W_)(R1.cl->payload[3]);
  Sp[-UF_SIZE-4]=(W_)(R1.cl->payload[2]);
  Sp[-UF_SIZE-5]=(W_)(R1.cl->payload[1]);
  R1.p=(P_)(R1.cl->payload[0]);
  Sp = Sp - (sizeofW(StgUpdateFrame)+5);
  JMP_(ENTRY_CODE(*R1.p));
  FE_
}

INFO_TABLE_SRT(__ap_7_upd_info,__ap_7_upd_entry,7,0,0,0,0,THUNK,,EF_,0,0);
FN_(__ap_7_upd_entry) {
  FB_
  STK_CHK_NP(sizeofW(StgUpdateFrame)+6,1,);
  UPD_BH_UPDATABLE(&__ap_7_upd_info);
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
  JMP_(ENTRY_CODE(*R1.p));
  FE_
}

INFO_TABLE_SRT(__ap_8_upd_info,__ap_8_upd_entry,8,0,0,0,0,THUNK,,EF_,0,0);
FN_(__ap_8_upd_entry) {
  FB_
  STK_CHK_NP(sizeofW(StgUpdateFrame)+7,1,);
  UPD_BH_UPDATABLE(&__ap_8_upd_info);
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
  Sp=Sp-10;
  JMP_(ENTRY_CODE(*R1.p));
  FE_
}
