/* ----------------------------------------------------------------------------
 * $Id: InfoMacros.h,v 1.8 1999/11/30 11:44:32 simonmar Exp $
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

/* function/thunk info tables --------------------------------------------- */

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


/* direct-return address info tables  --------------------------------------*/

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

/* info-table without an SRT -----------------------------------------------*/

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

/* special selector-thunk info table ---------------------------------------*/

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
