/* ----------------------------------------------------------------------------
 * $Id: InfoMacros.h,v 1.2 1998/12/02 13:21:09 simonm Exp $
 * 
 * Macros for building and deconstructing info tables.
 *
 * -------------------------------------------------------------------------- */

#ifndef INFOMACROS_H
#define INFOMACROS_H

#define STD_INFO(type_)				\
		srt : 0,			\
		srt_len : 0,			\
		type : type_,			\
		flags: FLAGS_##type_

#define SRT_INFO(type_,srt_,srt_off_,srt_len_)			\
		srt : (StgSRT *)((StgClosure **)srt_+srt_off_),	\
		srt_len : srt_len_,				\
		type : type_,					\
		flags: FLAGS_##type_

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
	info_class StgInfoTable info = {				\
		layout : { payload : {ptrs,nptrs} },			\
		SRT_INFO(type,srt_,srt_off_,srt_len_),			\
                INIT_ENTRY(entry)                                       \
	}


/* direct-return address info tables  --------------------------------------*/

#define \
INFO_TABLE_SRT_BITMAP(info, entry, bitmap_, srt_, srt_off_, srt_len_, \
		      type, info_class, entry_class,    \
		      prof_descr, prof_type)            \
        entry_class(entry);                             \
	info_class StgInfoTable info = {		\
		layout : { bitmap : (StgNat32)bitmap_ },\
		SRT_INFO(type,srt_,srt_off_,srt_len_),	\
                INIT_ENTRY(entry)                       \
	}

/* info-table without an SRT -----------------------------------------------*/

#define \
INFO_TABLE(info, entry, ptrs, nptrs, type, info_class, \
	   entry_class, prof_descr, prof_type) \
        entry_class(entry);                             \
	info_class StgInfoTable info = {		\
		layout : { payload : {ptrs,nptrs} }, 	\
		STD_INFO(type),				\
                INIT_ENTRY(entry)                       \
	}

/* special selector-thunk info table ---------------------------------------*/

#define \
INFO_TABLE_SELECTOR(info, entry, offset, info_class, \
		    entry_class, prof_descr, prof_type) \
        entry_class(entry);                                     \
	info_class StgInfoTable info = {			\
		layout : { selector_offset : offset },  \
		STD_INFO(THUNK_SELECTOR),		\
                INIT_ENTRY(entry)                       \
	}

/* constructor info table --------------------------------------------------*/

#define \
INFO_TABLE_CONSTR(info, entry, ptrs, nptrs, tag_,type_,info_class, \
		  entry_class, prof_descr, prof_type) \
        entry_class(entry);                             \
	info_class StgInfoTable info = {		\
		layout : { payload : {ptrs,nptrs} },	\
		srt_len : tag_,				\
		type : type_,				\
		flags : FLAGS_##type_,			\
                INIT_ENTRY(entry)                       \
	}

#define constrTag(con) (get_itbl(con)->srt_len)

/* return-vectors ----------------------------------------------------------*/

/* vectored-return info tables have the vector slammed up against the
 * start of the info table.
 *
 * A vectored-return address always has an SRT and a bitmap-style
 * layout field, so we only need one macro for these.
 */

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

#define VEC_INFO_TABLE(bitmap_,srt_,srt_off_,srt_len_,type)	\
	i : {						    	\
		layout : { bitmap : (StgNat32)bitmap_ },	\
		SRT_INFO(type,srt_,srt_off_,srt_len_)		\
	}

/* For polymorphic activation records, we need both a direct return
 * address and a return vector:
 */

#ifdef USE_MINIINTERPRETER
typedef StgInfoTable StgPolyInfoTable;
#define POLY_VEC(nm) \
  {                                                     \
	(F_) nm##_0_entry,				\
	(F_) nm##_1_entry,				\
	(F_) nm##_2_entry,				\
	(F_) nm##_3_entry,				\
	(F_) nm##_4_entry,				\
	(F_) nm##_5_entry,				\
	(F_) nm##_6_entry,				\
	(F_) nm##_7_entry                               \
   }
#define VEC_POLY_INFO_TABLE(nm,bitmap_,srt_,srt_off_,srt_len_,type) \
  StgFunPtr nm##_vec[8] = POLY_VEC(nm);                         \
  const StgInfoTable nm##_info = {				    	\
		layout : { bitmap : (StgNat32)bitmap_ },	\
		SRT_INFO(type,srt_,srt_off_,srt_len_),		\
		vector : &nm##_vec,                              \
                INIT_ENTRY(nm##_entry)                          \
	    }
#else
typedef vec_info_8 StgPolyInfoTable;
#define POLY_VEC(nm) \
  {                                                     \
	(F_) nm##_7_entry,				\
	(F_) nm##_6_entry,				\
	(F_) nm##_5_entry,				\
	(F_) nm##_4_entry,				\
	(F_) nm##_3_entry,				\
	(F_) nm##_2_entry,				\
	(F_) nm##_1_entry,				\
	(F_) nm##_0_entry                               \
   }
#define VEC_POLY_INFO_TABLE(nm,bitmap_,srt_,srt_off_,srt_len_,type) \
  const vec_info_8 nm##_info = {                                \
	vec : POLY_VEC(nm),                                     \
	i : {						    	\
		layout : { bitmap : (StgNat32)bitmap_ },	\
		SRT_INFO(type,srt_,srt_off_,srt_len_),		\
                INIT_ENTRY(nm##_entry)                          \
	    }                                                   \
  }
#endif

#define SRT(lbl) \
  static const StgSRT lbl = {

#define BITMAP(lbl,size) \
  static const StgLargeBitmap lbl = { size, {

#endif /* INFOMACROS_H */
