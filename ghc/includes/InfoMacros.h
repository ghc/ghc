/* ----------------------------------------------------------------------------
 * $Id: InfoMacros.h,v 1.10 2000/03/31 03:09:35 hwloidl Exp $
 * 
 * (c) The GHC Team, 1998-1999
 *
 * Macros for building and deconstructing info tables.
 *
 * -------------------------------------------------------------------------- */

#ifndef INFOMACROS_H
#define INFOMACROS_H

#define STD_INFO(type_)				\
		srt : 0,			\
		srt_len : 0,			\
		type : type_

#define SRT_INFO(type_,srt_,srt_off_,srt_len_)			\
		srt : (StgSRT *)((StgClosure **)srt_+srt_off_),	\
		srt_len : srt_len_,				\
		type : type_

#define CONSTR_INFO(type_,tag_)			\
		srt : 0,			\
		srt_len : tag_,			\
		type : type_

#ifdef USE_MINIINTERPRETER
#define INIT_VECTOR {}
#else
#define INIT_VECTOR
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
INFO_TABLE_SRT(info,				/* info-table label */	\
	       entry,				/* entry code label */	\
	       ptrs, nptrs,			/* closure layout info */\
	       srt_, srt_off_, srt_len_,	/* SRT info */		\
	       type,				/* closure type */	\
	       info_class, entry_class,		/* C storage classes */	\
	       prof_descr, prof_type)		/* profiling info */	\
        entry_class(RBH_##entry);                                      \
        entry_class(entry);                                             \
	ED_RO_ StgInfoTable info; \
	info_class INFO_TBL_CONST StgInfoTable RBH_##info = {		\
		layout : { payload : {ptrs,nptrs} },			\
		SRT_INFO(RBH,srt_,srt_off_,srt_len_),                  \
                INCLUDE_RBH_INFO(info),			                \
                INIT_ENTRY(RBH_##entry),                           \
                INIT_VECTOR                                             \
	} ; \
        StgFunPtr  RBH_##entry (void) { JMP_(RBH_entry); } ;            \
	info_class INFO_TBL_CONST StgInfoTable info = {			\
		layout : { payload : {ptrs,nptrs} },			\
		SRT_INFO(type,srt_,srt_off_,srt_len_),			\
                INCLUDE_RBH_INFO(RBH_##info),			\
                INIT_ENTRY(entry),                                      \
                INIT_VECTOR                                             \
	}

#else

#define \
INFO_TABLE_SRT(info,				/* info-table label */	\
	       entry,				/* entry code label */	\
	       ptrs, nptrs,			/* closure layout info */\
	       srt_, srt_off_, srt_len_,	/* SRT info */		\
	       type,				/* closure type */	\
	       info_class, entry_class,		/* C storage classes */	\
	       prof_descr, prof_type)		/* profiling info */	\
        entry_class(entry);                                             \
	info_class INFO_TBL_CONST StgInfoTable info = {			\
		layout : { payload : {ptrs,nptrs} },			\
		SRT_INFO(type,srt_,srt_off_,srt_len_),			\
                INIT_ENTRY(entry),                                      \
                INIT_VECTOR                                             \
	}

#endif

/* direct-return address info tables  --------------------------------------*/

#if defined(GRAN) || defined(PAR)

#define									\
INFO_TABLE_SRT_BITMAP(info, entry, bitmap_, srt_, srt_off_, srt_len_,	\
		      type, info_class, entry_class,			\
		      prof_descr, prof_type)				\
        entry_class(RBH_##entry);                                      \
        entry_class(entry);                                             \
	ED_RO_ StgInfoTable info; \
	info_class INFO_TBL_CONST StgInfoTable RBH_##info = {		\
		layout : { bitmap : (StgWord32)bitmap_ },		\
		SRT_INFO(RBH,srt_,srt_off_,srt_len_),			\
                INCLUDE_RBH_INFO(info),			                \
                INIT_ENTRY(RBH_##entry),				\
                INIT_VECTOR						\
	};                                                              \
        StgFunPtr  RBH_##entry (void) { JMP_(RBH_entry); } ;            \
	info_class INFO_TBL_CONST StgInfoTable info = {			\
		layout : { bitmap : (StgWord32)bitmap_ },		\
		SRT_INFO(type,srt_,srt_off_,srt_len_),			\
                INCLUDE_RBH_INFO(RBH_##info),		                \
                INIT_ENTRY(entry),					\
                INIT_VECTOR						\
	}
#else

#define									\
INFO_TABLE_SRT_BITMAP(info, entry, bitmap_, srt_, srt_off_, srt_len_,	\
		      type, info_class, entry_class,			\
		      prof_descr, prof_type)				\
        entry_class(entry);						\
	info_class INFO_TBL_CONST StgInfoTable info = {			\
		layout : { bitmap : (StgWord32)bitmap_ },		\
		SRT_INFO(type,srt_,srt_off_,srt_len_),			\
                INIT_ENTRY(entry),					\
                INIT_VECTOR						\
	}
#endif

/* info-table without an SRT -----------------------------------------------*/

#if defined(GRAN) || defined(PAR)

#define							\
INFO_TABLE(info, entry, ptrs, nptrs, type, info_class,	\
	   entry_class, prof_descr, prof_type)		\
        entry_class(RBH_##entry);                                      \
        entry_class(entry);                                             \
	ED_RO_ StgInfoTable info; \
	info_class INFO_TBL_CONST StgInfoTable RBH_##info = {	\
		layout : { payload : {ptrs,nptrs} },	\
		STD_INFO(RBH),				\
                INCLUDE_RBH_INFO(info),	                \
                INIT_ENTRY(RBH_##entry),		\
                INIT_VECTOR				\
	};                                              \
        StgFunPtr  RBH_##entry (void) { JMP_(RBH_entry); } ;            \
	info_class INFO_TBL_CONST StgInfoTable info = {	\
		layout : { payload : {ptrs,nptrs} },	\
		STD_INFO(type),				\
                INCLUDE_RBH_INFO(RBH_##info),		                \
                INIT_ENTRY(entry),			\
                INIT_VECTOR				\
	}

#else

#define							\
INFO_TABLE(info, entry, ptrs, nptrs, type, info_class,	\
	   entry_class, prof_descr, prof_type)		\
        entry_class(entry);				\
	info_class INFO_TBL_CONST StgInfoTable info = {	\
		layout : { payload : {ptrs,nptrs} },	\
		STD_INFO(type),				\
                INIT_ENTRY(entry),			\
                INIT_VECTOR				\
	}

#endif

/* special selector-thunk info table ---------------------------------------*/

#if defined(GRAN) || defined(PAR)

#define							\
INFO_TABLE_SELECTOR(info, entry, offset, info_class,	\
		    entry_class, prof_descr, prof_type)	\
        entry_class(RBH_##entry);                                      \
        entry_class(entry);                                             \
	ED_RO_ StgInfoTable info; \
	info_class INFO_TBL_CONST StgInfoTable RBH_##info = {	\
		layout : { selector_offset : offset },	\
		STD_INFO(RBH),		                \
                INCLUDE_RBH_INFO(info),	                \
                INIT_ENTRY(RBH_##entry),		\
                INIT_VECTOR				\
	};                                              \
        StgFunPtr  RBH_##entry (void) { JMP_(RBH_entry); } ;            \
	info_class INFO_TBL_CONST StgInfoTable info = {	\
		layout : { selector_offset : offset },	\
		STD_INFO(THUNK_SELECTOR),		\
                INCLUDE_RBH_INFO(RBH_##info),           \
                INIT_ENTRY(entry),			\
                INIT_VECTOR				\
	}

#else

#define							\
INFO_TABLE_SELECTOR(info, entry, offset, info_class,	\
		    entry_class, prof_descr, prof_type)	\
        entry_class(entry);				\
	info_class INFO_TBL_CONST StgInfoTable info = {	\
		layout : { selector_offset : offset },	\
		STD_INFO(THUNK_SELECTOR),		\
                INIT_ENTRY(entry),			\
                INIT_VECTOR				\
	}

#endif

/* constructor info table --------------------------------------------------*/

#define \
INFO_TABLE_CONSTR(info, entry, ptrs, nptrs, tag_,type_,info_class,	\
		  entry_class, prof_descr, prof_type)			\
        entry_class(entry);						\
	info_class INFO_TBL_CONST StgInfoTable info = {			\
		layout : { payload : {ptrs,nptrs} },			\
                CONSTR_INFO(type_,tag_),				\
                INIT_ENTRY(entry),					\
                INIT_VECTOR						\
	}

#define constrTag(con) (get_itbl(con)->srt_len)

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
  StgInfoTable i;
} vec_info_2;

typedef struct {
  StgFunPtr vec[3];
  StgInfoTable i;
} vec_info_3;

typedef struct {
  StgFunPtr vec[4];
  StgInfoTable i;
} vec_info_4;

typedef struct {
  StgFunPtr vec[5];
  StgInfoTable i;
} vec_info_5;

typedef struct {
  StgFunPtr vec[6];
  StgInfoTable i;
} vec_info_6;

typedef struct {
  StgFunPtr vec[7];
  StgInfoTable i;
} vec_info_7;

typedef struct {
  StgFunPtr vec[8];
  StgInfoTable i;
} vec_info_8;

#define VEC_INFO_2(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2)				\
  	info_class INFO_TBL_CONST vec_info_2 info = {		\
		{ alt_2, alt_1 },				\
		i : {						\
		   layout : { bitmap : (StgWord32)bitmap_ },	\
		   SRT_INFO(type,srt_,srt_off_,srt_len_)	\
		}						\
	}

#define VEC_INFO_3(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3				\
		  )						\
  	info_class INFO_TBL_CONST vec_info_3 info = {		\
		{ alt_3, alt_2, alt_1 },			\
		i : {						\
		   layout : { bitmap : (StgWord32)bitmap_ },	\
		   SRT_INFO(type,srt_,srt_off_,srt_len_)	\
		}						\
	}

#define VEC_INFO_4(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4			\
		  )						\
  	info_class INFO_TBL_CONST vec_info_4 info = {		\
		{ alt_4, alt_3, alt_2, alt_1 },			\
		i : {						\
		   layout : { bitmap : (StgWord32)bitmap_ },	\
		   SRT_INFO(type,srt_,srt_off_,srt_len_)	\
		}						\
	}

#define VEC_INFO_5(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5					\
		  )						\
  	info_class INFO_TBL_CONST vec_info_5 info = {		\
		{ alt_5, alt_4, alt_3, alt_2, 			\
		  alt_1 },					\
		i : {						\
		   layout : { bitmap : (StgWord32)bitmap_ },	\
		   SRT_INFO(type,srt_,srt_off_,srt_len_)	\
		}						\
	}

#define VEC_INFO_6(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6					\
		  )						\
  	info_class INFO_TBL_CONST vec_info_6 info = {		\
		{ alt_6, alt_5, alt_4, alt_3,			\
		  alt_2, alt_1 },				\
		i : {						\
		   layout : { bitmap : (StgWord32)bitmap_ },	\
		   SRT_INFO(type,srt_,srt_off_,srt_len_)	\
		}						\
	}

#define VEC_INFO_7(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6, alt_7				\
		  )						\
  	info_class INFO_TBL_CONST vec_info_7 info = {		\
		{ alt_7, alt_6, alt_5, alt_4, 			\
		  alt_3, alt_2, alt_1 },			\
		i : {						\
		   layout : { bitmap : (StgWord32)bitmap_ },	\
		   SRT_INFO(type,srt_,srt_off_,srt_len_)	\
		}						\
	}

#define VEC_INFO_8(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6, alt_7, alt_8			\
		  )						\
  	info_class INFO_TBL_CONST vec_info_8 info = {		\
		{ alt_8, alt_7, alt_6, alt_5, 			\
		  alt_4, alt_3, alt_2, alt_1 },			\
		i : {						\
		   layout : { bitmap : (StgWord32)bitmap_ },	\
		   SRT_INFO(type,srt_,srt_off_,srt_len_)	\
		}						\
	}


#else

/* We have to define these structure to work around a bug in gcc: if we
 * try to initialise the vector directly (it's defined as a zero-length
 * array tacked on the end of the info table structor), then gcc silently
 * throws away our vector table sometimes.
 */

typedef struct {
  StgInfoTable i;
  StgFunPtr vec[2];
} vec_info_2;

typedef struct {
  StgInfoTable i;
  StgFunPtr vec[3];
} vec_info_3;

typedef struct {
  StgInfoTable i;
  StgFunPtr vec[4];
} vec_info_4;

typedef struct {
  StgInfoTable i;
  StgFunPtr vec[5];
} vec_info_5;

typedef struct {
  StgInfoTable i;
  StgFunPtr vec[6];
} vec_info_6;

typedef struct {
  StgInfoTable i;
  StgFunPtr vec[7];
} vec_info_7;

typedef struct {
  StgInfoTable i;
  StgFunPtr vec[8];
} vec_info_8;

#define VEC_INFO_2(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2)				\
	info_class INFO_TBL_CONST vec_info_2 info = {		\
		i : { layout : { bitmap : (StgWord32)bitmap_ },	\
		      SRT_INFO(type,srt_,srt_off_,srt_len_),	\
		      INIT_ENTRY(NULL),				\
		},						\
		vec : { alt_1, alt_2 }				\
	}

#define VEC_INFO_3(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3				\
		  )						\
	info_class INFO_TBL_CONST vec_info_3 info = {		\
		i : { layout : { bitmap : (StgWord32)bitmap_ },	\
		      SRT_INFO(type,srt_,srt_off_,srt_len_),	\
		      INIT_ENTRY(NULL),				\
		},						\
                vec : { alt_1, alt_2, alt_3 }			\
	}

#define VEC_INFO_4(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4			\
		  )						\
	info_class INFO_TBL_CONST vec_info_4 info = {		\
		i : { layout : { bitmap : (StgWord32)bitmap_ },	\
		      SRT_INFO(type,srt_,srt_off_,srt_len_),	\
		      INIT_ENTRY(NULL),				\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4 }		\
	}

#define VEC_INFO_5(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5					\
		  )						\
	info_class INFO_TBL_CONST vec_info_5 info = {		\
		i : { layout : { bitmap : (StgWord32)bitmap_ },	\
		      SRT_INFO(type,srt_,srt_off_,srt_len_),	\
		      INIT_ENTRY(NULL),				\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4,		\
			alt_5 }					\
	}

#define VEC_INFO_6(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6					\
		  )						\
	info_class INFO_TBL_CONST vec_info_6 info = {		\
		i : { layout : { bitmap : (StgWord32)bitmap_ },	\
		      SRT_INFO(type,srt_,srt_off_,srt_len_),	\
		      INIT_ENTRY(NULL),				\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4,		\
			alt_5, alt_6 }				\
	}

#define VEC_INFO_7(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6, alt_7				\
		  )						\
	info_class INFO_TBL_CONST vec_info_7 info = {		\
		i : { layout : { bitmap : (StgWord32)bitmap_ },	\
		      SRT_INFO(type,srt_,srt_off_,srt_len_),	\
		      INIT_ENTRY(NULL),				\
		},						\
                vec : { alt_1, alt_2, alt_3, alt_4,		\
			alt_5, alt_6, alt_7 }			\
	}

#define VEC_INFO_8(info,bitmap_,srt_,srt_off_,srt_len_,		\
		   type, info_class,				\
		   alt_1, alt_2, alt_3, alt_4,			\
		   alt_5, alt_6, alt_7, alt_8			\
		  )						\
	info_class INFO_TBL_CONST vec_info_8 info = {		\
		i : { layout : { bitmap : (StgWord32)bitmap_ },	\
		      SRT_INFO(type,srt_,srt_off_,srt_len_),	\
		      INIT_ENTRY(NULL),				\
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
			   srt_, srt_off_, srt_len_,		\
			   type, info_class, entry_class	\
			   )					\
  info_class INFO_TBL_CONST vec_info_8 nm##_info = {		\
		i : { layout : { bitmap : (StgWord32)bitmap_ },	\
		      SRT_INFO(type,srt_,srt_off_,srt_len_),	\
                      INIT_ENTRY(nm##_entry),			\
		      INIT_VECTOR				\
		},						\
		vec : {						\
			(F_) nm##_0_entry,			\
			(F_) nm##_1_entry,			\
			(F_) nm##_2_entry,			\
			(F_) nm##_3_entry,			\
			(F_) nm##_4_entry,			\
			(F_) nm##_5_entry,			\
			(F_) nm##_6_entry,			\
			(F_) nm##_7_entry			\
		}						\
	    }
#else

#define VEC_POLY_INFO_TABLE(nm, bitmap_,			\
			   srt_, srt_off_, srt_len_,		\
			   type, info_class, entry_class	\
			   )					\
  	info_class INFO_TBL_CONST vec_info_8 nm##_info = {	\
		{						\
			(F_) nm##_7_entry,			\
			(F_) nm##_6_entry,			\
			(F_) nm##_5_entry,			\
			(F_) nm##_4_entry,			\
			(F_) nm##_3_entry,			\
			(F_) nm##_2_entry,			\
			(F_) nm##_1_entry,			\
			(F_) nm##_0_entry			\
		},						\
		i : {						\
		   layout : { bitmap : (StgWord32)bitmap_ },	\
		   SRT_INFO(type,srt_,srt_off_,srt_len_),	\
                   INIT_ENTRY(nm##_entry)			\
		}						\
	}

#endif

#define SRT(lbl) \
  static const StgSRT lbl = {

#define BITMAP(lbl,size) \
  static const StgLargeBitmap lbl = { size, {

/* DLL_SRT_ENTRY is used on the Win32 side when filling initialising
   an entry in an SRT table with a reference to a closure that's
   living in a DLL. See elsewhere for reasons as to why we need
   to distinguish these kinds of references.
   (ToDo: fill in a more precise href.)
*/
#ifdef HAVE_WIN32_DLL_SUPPORT
#define DLL_SRT_ENTRY(x) ((StgClosure*)(((char*)&DLL_IMPORT_DATA_VAR(x)) + 1))
#else
#define DLL_SRT_ENTRY(x) no-can-do
#endif

#endif /* INFOMACROS_H */
