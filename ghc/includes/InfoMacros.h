/* ----------------------------------------------------------------------------
 * $Id: InfoMacros.h,v 1.22 2003/05/14 09:14:01 simonmar Exp $
 * 
 * (c) The GHC Team, 1998-2002
 *
 * Macros for building and deconstructing info tables.
 *
 * -------------------------------------------------------------------------- */

#ifndef INFOMACROS_H
#define INFOMACROS_H

#define STD_INFO(srt_bitmap_, type_)		\
		srt_bitmap : srt_bitmap_,		\
		type : type_

#define THUNK_INFO(srt_, srt_off_)			\
		srt : (StgSRT *)((StgClosure **)srt_+srt_off_)

#define FUN_GEN_INFO(srt_, srt_off_, fun_type_, arity_, bitmap_, slow_apply_) \

#define RET_INFO(srt_, srt_off_)				\
		srt : (StgSRT *)((StgClosure **)srt_+srt_off_)

#ifdef PROFILING
#define PROF_INFO(type_str, desc_str)		\
		prof: {				\
		   closure_type: type_str,	\
		   closure_desc: desc_str,	\
		},
#else
#define PROF_INFO(type_str, desc_str)
#endif

/*
  On the GranSim/GUM specific parts of the InfoTables (GRAN/PAR):

  In both GranSim and GUM we use revertible black holes (RBH) when putting
  an updatable closure into a packet for communication. The entry code for
  an RBH performs standard blocking (as with any kind of BH). The info
  table for the RBH resides just before the one for the std info
  table. (NB: there is one RBH ITBL for every ITBL of an updatable
  closure.) The @rbh_infoptr@ field in the ITBL points from the std ITBL to
  the RBH ITBL and vice versa. This is used by the RBH_INFOPTR and
  REVERT_INFOPTR macros to turn an updatable node into an RBH and vice
  versa. Note, that the only case where we have to revert the RBH in its
  original form is when a packet is sent back because of garbage collection
  on another PE. In the RTS for GdH we will use this reversion mechanism in 
  order to deal with faults in the system. 
  ToDo: Check that RBHs are needed for all the info tables below. From a quick
  check of the macros generated in the libs it seems that all of them are used
  for generating THUNKs.
  Possible optimisation: Note that any RBH ITBL is a fixed distance away from 
  the actual ITBL. We could inline this offset as a constant into the RTS and
  avoid the rbh_infoptr fields altogether (Jim did that in the old RTS).
  -- HWL
*/


/* function/thunk info tables --------------------------------------------- */

#if defined(GRAN) || defined(PAR)

#define \
INFO_TABLE_THUNK(info,				/* info-table label */	\
	       entry,				/* entry code label */	\
	       ptrs, nptrs,			/* closure layout info */\
	       srt_, srt_off_, srt_bitmap_,	/* SRT info */		\
	       type,				/* closure type */	\
	       info_class, entry_class,		/* C storage classes */	\
	       prof_descr, prof_type)		/* profiling info */	\
        entry_class(stg_RBH_##entry);                                      	\
        entry_class(entry);                                             \
	ED_RO_ StgInfoTable info; 					\
	info_class const StgInfoTable stg_RBH_##info = {		\
		layout : { payload : {ptrs,nptrs} },			\
                PROF_INFO(prof_type, prof_descr)			\
		SRT_INFO(RBH,srt_,srt_off_,srt_bitmap_),                  	\
                INCLUDE_RBH_INFO(info),			                \
                INIT_ENTRY(stg_RBH_##entry)                           	\
	} ; 								\
        StgFunPtr stg_RBH_##entry (void) {                                  \
          FB_                                                           \
            JMP_(stg_RBH_entry);                                            \
          FE_                                                           \
        } ;                                                             \
	info_class const StgInfoTable info = {			\
		layout : { payload : {ptrs,nptrs} },			\
                PROF_INFO(prof_type, prof_descr)			\
		SRT_INFO(type,srt_,srt_off_,srt_bitmap_),			\
                INCLUDE_RBH_INFO(stg_RBH_##info),			\
                INIT_ENTRY(entry)                                       \
	}

#else

#define \
INFO_TABLE_THUNK(info,				/* info-table label */	\
	       entry,				/* entry code label */	\
	       ptrs, nptrs,			/* closure layout info */\
	       srt_, srt_off_, srt_bitmap_,	/* SRT info */		\
	       type_,				/* closure type */	\
	       info_class, entry_class,		/* C storage classes */	\
	       prof_descr, prof_type)		/* profiling info */	\
        entry_class(entry);                                             \
	info_class const StgThunkInfoTable info = {		\
		i : {							\
		  layout : { payload : {ptrs,nptrs} },			\
                  PROF_INFO(prof_type, prof_descr)			\
		  STD_INFO(srt_bitmap_, type_),				\
                  INIT_ENTRY(entry)                                     \
		},							\
		THUNK_INFO(srt_,srt_off_),				\
	}

#endif

/* direct-return address info tables  --------------------------------------*/

#if defined(GRAN) || defined(PAR)

#define									\
INFO_TABLE_RET(info, entry, bitmap_, srt_, srt_off_, srt_bitmap_,		\
		      type, info_class, entry_class,			\
		      prof_descr, prof_type)				\
        entry_class(stg_RBH_##entry);					\
        entry_class(entry);						\
	ED_RO_ StgInfoTable info;					\
	info_class const StgInfoTable stg_RBH_##info = {	\
		layout : { bitmap : (StgWord)bitmap_ },			\
                PROF_INFO(prof_type, prof_descr)			\
		SRT_INFO(RBH,srt_,srt_off_,srt_bitmap_),			\
                INCLUDE_RBH_INFO(info),					\
                INIT_ENTRY(stg_RBH_##entry)				\
	};								\
        StgFunPtr stg_RBH_##entry (void) {                              \
          FB_                                                           \
            JMP_(stg_RBH_entry);                                        \
          FE_                                                           \
        } ;                                                             \
	info_class const StgInfoTable info = {			\
		layout : { bitmap : (StgWord)bitmap_ },			\
                PROF_INFO(prof_type, prof_descr)			\
		SRT_INFO(type,srt_,srt_off_,srt_bitmap_),			\
                INCLUDE_RBH_INFO(stg_RBH_##info),			\
                INIT_ENTRY(entry)					\
	}

#else

#define									\
INFO_TABLE_RET(info, entry, bitmap_, srt_, srt_off_, srt_bitmap_,		\
		      type_, info_class, entry_class,			\
		      prof_descr, prof_type)				\
        entry_class(entry);						\
	info_class const StgRetInfoTable info = {		\
		i : {							\
		    layout : { bitmap : (StgWord)bitmap_ },		\
		    PROF_INFO(prof_type, prof_descr)			\
		    STD_INFO(srt_bitmap_,type_),				\
                    INIT_ENTRY(entry)					\
 	        },							\
		RET_INFO(srt_,srt_off_)					\
	}
#endif

/* info-table without an SRT -----------------------------------------------*/

#if defined(GRAN) || defined(PAR)

#define								\
INFO_TABLE(info, entry, ptrs, nptrs, type, info_class,		\
	   entry_class, prof_descr, prof_type)			\
        entry_class(stg_RBH_##entry);				\
        entry_class(entry);					\
	ED_ StgInfoTable info;				\
	info_class const StgInfoTable stg_RBH_##info = {	\
		layout : { payload : {ptrs,nptrs} },		\
                PROF_INFO(prof_type, prof_descr)		\
		STD_INFO(RBH),					\
                INCLUDE_RBH_INFO(info),			        \
                INIT_ENTRY(stg_RBH_##entry)		        \
	} ;                                                     \
        StgFunPtr stg_RBH_##entry (void) {                      \
          FB_                                                   \
            JMP_(stg_RBH_entry);                                \
          FE_                                                   \
        } ;                                                     \
	info_class const StgInfoTable info = {	\
		layout : { payload : {ptrs,nptrs} },		\
                PROF_INFO(prof_type, prof_descr)		\
		STD_INFO(type),					\
                INCLUDE_RBH_INFO(stg_RBH_##info),		\
                INIT_ENTRY(entry)				\
	}

#else

#define							\
INFO_TABLE(info, entry, ptrs, nptrs, type, info_class,	\
	   entry_class, prof_descr, prof_type)		\
        entry_class(entry);				\
	info_class const StgInfoTable info = {	\
		layout : { payload : {ptrs,nptrs} },	\
                PROF_INFO(prof_type, prof_descr)	\
		STD_INFO(0, type),			\
                INIT_ENTRY(entry)			\
	}

#endif

/* special selector-thunk info table ---------------------------------------*/

#if defined(GRAN) || defined(PAR)

#define								\
INFO_TABLE_SELECTOR(info, entry, offset, info_class,		\
		    entry_class, prof_descr, prof_type)		\
        entry_class(stg_RBH_##entry);				\
        entry_class(entry);					\
	ED_RO_ StgInfoTable info;				\
	info_class const StgInfoTable stg_RBH_##info = {	\
		layout : { selector_offset : offset },		\
                PROF_INFO(prof_type, prof_descr)		\
		STD_INFO(RBH),					\
                INCLUDE_RBH_INFO(info),				\
                INIT_ENTRY(stg_RBH_##entry)			\
	};							\
        StgFunPtr stg_RBH_##entry (void) {                          \
          FB_                                                   \
            JMP_(stg_RBH_entry);                                    \
          FE_                                                   \
        } ;                                                     \
	info_class const StgInfoTable info = {		\
		layout : { selector_offset : offset },		\
                PROF_INFO(prof_type, prof_descr)		\
		STD_INFO(THUNK_SELECTOR),			\
                INCLUDE_RBH_INFO(stg_RBH_##info),			\
                INIT_ENTRY(entry)				\
	}

#else

#define							\
INFO_TABLE_SELECTOR(info, entry, offset, info_class,	\
		    entry_class, prof_descr, prof_type)	\
        entry_class(entry);				\
	info_class const StgInfoTable info = {	\
		layout : { selector_offset : offset },	\
                PROF_INFO(prof_type, prof_descr)	\
		STD_INFO(0,THUNK_SELECTOR),		\
                INIT_ENTRY(entry)			\
	}

#endif

/* constructor info table --------------------------------------------------*/

#define									\
INFO_TABLE_CONSTR(info, entry, ptrs, nptrs, tag_,type_,info_class,	\
		  entry_class, prof_descr, prof_type)			\
        entry_class(entry);						\
	info_class const StgInfoTable info = {			\
		layout : { payload : {ptrs,nptrs} },			\
                PROF_INFO(prof_type, prof_descr)			\
                STD_INFO(tag_, type_),					\
                INIT_ENTRY(entry)					\
	}

#define constrTag(con) (get_itbl(con)->srt_bitmap)

/* function info table -----------------------------------------------------*/

#define									\
INFO_TABLE_FUN_GEN(info,			/* info-table label */	\
	       entry,				/* entry code label */	\
	       ptrs, nptrs,			/* closure layout info */\
	       srt_, srt_off_, srt_bitmap_,	/* SRT info */		\
	       fun_type_, arity_, bitmap_, slow_apply_,			\
		   				/* Function info */     \
	       type_,				/* closure type */	\
	       info_class, entry_class,		/* C storage classes */	\
	       prof_descr, prof_type)		/* profiling info */	\
        entry_class(entry);                                             \
	info_class const StgFunInfoTable info = {		\
                i : {							\
		   layout : { payload : {ptrs,nptrs} },			\
                   PROF_INFO(prof_type, prof_descr)			\
		   STD_INFO(srt_bitmap_,type_),				\
	           INIT_ENTRY(entry)                                    \
		},							\
		srt : (StgSRT *)((StgClosure **)srt_+srt_off_),		\
                arity : arity_,						\
                fun_type : fun_type_,					\
	 	bitmap : (W_)bitmap_,					\
	 	slow_apply : slow_apply_				\
	}

/* return-vectors ----------------------------------------------------------*/

/* vectored-return info tables have the vector slammed up against the
 * start of the info table.
 *
 * A vectored-return address always has an SRT and a bitmap-style
 * layout field, so we only need one macro for these.
 */

#ifdef TABLES_NEXT_TO_CODE

typedef struct {
  StgFunPtr vec[2];
  StgRetInfoTable i;
} vec_info_2;

typedef struct {
  StgFunPtr vec[3];
  StgRetInfoTable i;
} vec_info_3;

typedef struct {
  StgFunPtr vec[4];
  StgRetInfoTable i;
} vec_info_4;

typedef struct {
  StgFunPtr vec[5];
  StgRetInfoTable i;
} vec_info_5;

typedef struct {
  StgFunPtr vec[6];
  StgRetInfoTable i;
} vec_info_6;

typedef struct {
  StgFunPtr vec[7];
  StgRetInfoTable i;
} vec_info_7;

typedef struct {
  StgFunPtr vec[8];
  StgRetInfoTable i;
} vec_info_8;

#define VEC_INFO_2(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2)				\
  	info_class const vec_info_2 info = {		\
		{ alt_2, alt_1 },				\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		}						\
	}

#define VEC_INFO_3(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3				\
		  )						\
  	info_class const vec_info_3 info = {		\
		{ alt_3, alt_2, alt_1 },			\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		}						\
	}

#define VEC_INFO_4(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4			\
		  )						\
  	info_class const vec_info_4 info = {		\
		{ alt_4, alt_3, alt_2, alt_1 },			\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		}						\
	}

#define VEC_INFO_5(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5					\
		  )						\
  	info_class const vec_info_5 info = {		\
		{ alt_5, alt_4, alt_3, alt_2, 			\
		  alt_1 },					\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		}						\
	}

#define VEC_INFO_6(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6					\
		  )						\
  	info_class const vec_info_6 info = {		\
		{ alt_6, alt_5, alt_4, alt_3,			\
		  alt_2, alt_1 },				\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		}						\
	}

#define VEC_INFO_7(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6, alt_7				\
		  )						\
  	info_class const vec_info_7 info = {		\
		{ alt_7, alt_6, alt_5, alt_4, 			\
		  alt_3, alt_2, alt_1 },			\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		}						\
	}

#define VEC_INFO_8(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6, alt_7, alt_8			\
		  )						\
  	info_class const vec_info_8 info = {		\
		{ alt_8, alt_7, alt_6, alt_5, 			\
		  alt_4, alt_3, alt_2, alt_1 },			\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		}						\
	}


#else

/* We have to define these structure to work around a bug in gcc: if we
 * try to initialise the vector directly (it's defined as a zero-length
 * array tacked on the end of the info table structor), then gcc silently
 * throws away our vector table sometimes.
 */

typedef struct {
  StgRetInfoTable i;
  StgFunPtr vec[2];
} vec_info_2;

typedef struct {
  StgRetInfoTable i;
  StgFunPtr vec[3];
} vec_info_3;

typedef struct {
  StgRetInfoTable i;
  StgFunPtr vec[4];
} vec_info_4;

typedef struct {
  StgRetInfoTable i;
  StgFunPtr vec[5];
} vec_info_5;

typedef struct {
  StgRetInfoTable i;
  StgFunPtr vec[6];
} vec_info_6;

typedef struct {
  StgRetInfoTable i;
  StgFunPtr vec[7];
} vec_info_7;

typedef struct {
  StgRetInfoTable i;
  StgFunPtr vec[8];
} vec_info_8;

#define VEC_INFO_2(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2)				\
	info_class const vec_info_2 info = {		\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		}						\
	}

#define VEC_INFO_3(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3				\
		  )						\
	info_class const vec_info_3 info = {		\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		},						\
                vec : { alt_1, alt_2, alt_3 }			\
	}

#define VEC_INFO_4(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4			\
		  )						\
	info_class const vec_info_4 info = {		\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4 }		\
	}

#define VEC_INFO_5(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5					\
		  )						\
	info_class const vec_info_5 info = {		\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4,		\
			alt_5 }					\
	}

#define VEC_INFO_6(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6					\
		  )						\
	info_class const vec_info_6 info = {		\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4,		\
			alt_5, alt_6 }				\
	}

#define VEC_INFO_7(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6, alt_7				\
		  )						\
	info_class const vec_info_7 info = {		\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4,		\
			alt_5, alt_6, alt_7 }			\
	}

#define VEC_INFO_8(info,bitmap_,srt_,srt_off_,srt_bitmap_,		\
		   type_, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6, alt_7, alt_8			\
		  )						\
	info_class const vec_info_8 info = {		\
		i : {						\
		   i : {					\
		      layout : { bitmap : (StgWord)bitmap_ },	\
		      STD_INFO(srt_bitmap_,type_)			\
		   },						\
		   RET_INFO(srt_,srt_off_)			\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4,		\
			alt_5, alt_6, alt_7, alt_8 }		\
	}

#endif /* TABLES_NEXT_TO_CODE */

/* For polymorphic activation records, we need both a direct return
 * address and a return vector:
 */

typedef vec_info_8 StgPolyInfoTable;

#ifndef TABLES_NEXT_TO_CODE

#define VEC_POLY_INFO_TABLE(nm, bitmap_, 			\
			   srt_, srt_off_, srt_bitmap_,		\
			   type_, info_class, entry_class	\
			   )					\
  info_class const vec_info_8 nm##_info = {			\
		i : { 						\
		    i : { 					\
			layout : { 				\
			bitmap : (StgWord)bitmap_ },		\
			STD_INFO(srt_bitmap_, type_),		\
                      	INIT_ENTRY(nm##_ret)			\
		    },						\
		    RET_INFO(srt_,srt_off_)			\
		},						\
		vec : {						\
			(F_) nm##_0_ret,			\
			(F_) nm##_1_ret,			\
			(F_) nm##_2_ret,			\
			(F_) nm##_3_ret,			\
			(F_) nm##_4_ret,			\
			(F_) nm##_5_ret,			\
			(F_) nm##_6_ret,			\
			(F_) nm##_7_ret				\
		}						\
	    }
#else

#define VEC_POLY_INFO_TABLE(nm, bitmap_, 			\
			   srt_, srt_off_, srt_bitmap_,		\
			   type_, info_class, entry_class	\
			   )					\
  	info_class const vec_info_8 nm##_info = {	\
		{						\
			(F_) nm##_7_ret,			\
			(F_) nm##_6_ret,			\
			(F_) nm##_5_ret,			\
			(F_) nm##_4_ret,			\
			(F_) nm##_3_ret,			\
			(F_) nm##_2_ret,			\
			(F_) nm##_1_ret,			\
			(F_) nm##_0_ret			\
		},						\
		i : { 						\
		    i : { 					\
			layout : { 				\
			bitmap : (StgWord)bitmap_ },		\
			STD_INFO(srt_bitmap_, type_),		\
                      	INIT_ENTRY(nm##_ret)			\
		    },						\
		    RET_INFO(srt_,srt_off_)			\
		}						\
	}

#endif

#define SRT(lbl) \
  static const StgSRT lbl = {

/* DLL_SRT_ENTRY is used on the Win32 side when filling initialising
   an entry in an SRT table with a reference to a closure that's
   living in a DLL. See elsewhere for reasons as to why we need
   to distinguish these kinds of references.
   (ToDo: fill in a more precise href.)
*/
#ifdef ENABLE_WIN32_DLL_SUPPORT /* mingw DietHEP doesn't seem to care either way */
#define DLL_SRT_ENTRY(x) ((StgClosure*)(((char*)&DLL_IMPORT_DATA_VAR(x)) + 1))
#else
#define DLL_SRT_ENTRY(x) no-can-do
#endif

#endif /* INFOMACROS_H */
