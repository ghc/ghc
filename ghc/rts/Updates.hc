/* -----------------------------------------------------------------------------
 * $Id: Updates.hc,v 1.40 2003/05/14 09:14:00 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2002
 *
 * Code to perform updates.
 *
 * ---------------------------------------------------------------------------*/

#include "Stg.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Storage.h"
#if defined(GRAN) || defined(PAR)
# include "FetchMe.h"
#endif

/*
  The update frame return address must be *polymorphic*, that means
  we have to cope with both vectored and non-vectored returns.  This
  is done by putting the return vector right before the info table, and
  having a standard direct return address after the info table (pointed
  to by the return address itself, as usual).

  Each entry in the vector table points to a specialised entry code fragment
  that knows how to return after doing the update.  It would be possible to
  use a single generic piece of code that simply entered the return value
  to return, but it's quicker this way.  The direct return code of course
  just does another direct return when it's finished.
*/

/* on entry to the update code
   (1) R1 points to the closure being returned
   (2) Sp points to the update frame
   */

/* The update fragment has been tuned so as to generate reasonable
   code with gcc, which accounts for some of the strangeness in the
   way it is written.  

   In particular, the JMP_(ret) bit is passed down and pinned on the
   end of each branch (there end up being two major branches in the
   code), since we don't mind duplicating this jump.
*/

#define UPD_FRAME_ENTRY_TEMPLATE(label,ind_info,ret)			\
        STGFUN(label);							\
	STGFUN(label)							\
	{								\
          StgClosure *updatee;						\
	  FB_								\
									\
          updatee = ((StgUpdateFrame *)Sp)->updatee;			\
									\
	  /* remove the update frame from the stack */			\
	  Sp += sizeofW(StgUpdateFrame);				\
									\
	  /* Tick - it must be a con, all the paps are handled		\
	   * in stg_upd_PAP and PAP_entry below				\
	   */								\
	  TICK_UPD_CON_IN_NEW(sizeW_fromITBL(get_itbl(updatee)));	\
									\
	  UPD_SPEC_IND(updatee, ind_info, R1.cl, JMP_(ret));		\
	  FE_								\
	}

UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_ret,&stg_IND_direct_info,ENTRY_CODE(Sp[0]));
UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_0_ret,&stg_IND_0_info,RET_VEC(Sp[0],0));
UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_1_ret,&stg_IND_1_info,RET_VEC(Sp[0],1));
UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_2_ret,&stg_IND_2_info,RET_VEC(Sp[0],2));
UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_3_ret,&stg_IND_3_info,RET_VEC(Sp[0],3));
UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_4_ret,&stg_IND_4_info,RET_VEC(Sp[0],4));
UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_5_ret,&stg_IND_5_info,RET_VEC(Sp[0],5));
UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_6_ret,&stg_IND_6_info,RET_VEC(Sp[0],6));
UPD_FRAME_ENTRY_TEMPLATE(stg_upd_frame_7_ret,&stg_IND_7_info,RET_VEC(Sp[0],7));

/*
  Make sure this table is big enough to handle the maximum vectored
  return size!
  */

#if defined(PROFILING)
#define UPD_FRAME_BITMAP 3
#define UPD_FRAME_WORDS  3
#else
#define UPD_FRAME_BITMAP 0
#define UPD_FRAME_WORDS  1
#endif

/* this bitmap indicates that the first word of an update frame is a
 * non-pointer - this is the update frame link.  (for profiling,
 * there's a cost-centre-stack in there too).
 */

VEC_POLY_INFO_TABLE( stg_upd_frame, 
		     MK_SMALL_BITMAP(UPD_FRAME_WORDS, UPD_FRAME_BITMAP),
		     0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/,
		     UPDATE_FRAME,, EF_);

/*-----------------------------------------------------------------------------
  Seq frames 

  We don't have a primitive seq# operator: it is just a 'case'
  expression whose scrutinee has either a polymorphic or function type
  (constructor types can be handled by normal 'case' expressions).

  To handle a polymorphic/function typed seq, we push a SEQ frame on
  the stack.  This is a polymorphic activation record that just pops
  itself and returns (in a non-vectored way) when entered.  The
  purpose of the SEQ frame is to avoid having to make a polymorphic return
  point for each polymorphic case expression.  

  Another way of looking at it: the SEQ frame turns a vectored return
  into a direct one.
  -------------------------------------------------------------------------- */

IF_(stg_seq_frame_ret);

#define stg_seq_frame_0_ret stg_seq_frame_ret
#define stg_seq_frame_1_ret stg_seq_frame_ret
#define stg_seq_frame_2_ret stg_seq_frame_ret
#define stg_seq_frame_3_ret stg_seq_frame_ret
#define stg_seq_frame_4_ret stg_seq_frame_ret
#define stg_seq_frame_5_ret stg_seq_frame_ret
#define stg_seq_frame_6_ret stg_seq_frame_ret
#define stg_seq_frame_7_ret stg_seq_frame_ret

VEC_POLY_INFO_TABLE( stg_seq_frame,
		     MK_SMALL_BITMAP(0, 0),
		     0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/,
		     RET_SMALL,, EF_);

IF_(stg_seq_frame_ret)
{
   FB_
   Sp ++;
   JMP_(ENTRY_CODE(Sp[0]));
   FE_
}
