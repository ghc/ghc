/* -----------------------------------------------------------------------------
 * $Id: Prelude.h,v 1.3 1999/01/26 11:12:56 simonm Exp $
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRELUDE_H
#define PRELUDE_H

#ifdef COMPILER
extern const StgClosure PrelBase_Z91Z93_static_closure;
extern const StgClosure PrelBase_Z40Z41_static_closure;
extern const StgClosure PrelBase_True_static_closure;
extern const StgClosure PrelBase_False_static_closure;
extern const StgClosure PrelMain_mainIO_closure;
extern const StgClosure PrelPack_unpackCString_closure;

extern const StgInfoTable PrelBase_CZh_static_info;
extern const StgInfoTable PrelBase_IZh_static_info;
extern const StgInfoTable PrelBase_FZh_static_info;
extern const StgInfoTable PrelBase_DZh_static_info;
extern const StgInfoTable PrelAddr_AZh_static_info;
extern const StgInfoTable PrelAddr_WZh_static_info;
extern const StgInfoTable PrelBase_CZh_con_info;
extern const StgInfoTable PrelBase_IZh_con_info;
extern const StgInfoTable PrelBase_FZh_con_info;
extern const StgInfoTable PrelBase_DZh_con_info;
extern const StgInfoTable PrelAddr_AZh_con_info;
extern const StgInfoTable PrelAddr_WZh_con_info;
extern const StgInfoTable PrelAddr_I64Zh_con_info;
extern const StgInfoTable PrelAddr_W64Zh_con_info;
extern const StgInfoTable PrelStable_StablePtr_static_info;
extern const StgInfoTable PrelStable_StablePtr_con_info;

/* Define canonical names so we can abstract away from the actual
 * module these names are defined in.
 */

#define Nil_closure           PrelBase_Z91Z93_static_closure
#define Unit_closure          PrelBase_Z40Z41_static_closure
#define True_closure          PrelBase_True_static_closure
#define False_closure         PrelBase_False_static_closure
#define CZh_static_info       PrelBase_CZh_static_info
#define IZh_static_info       PrelBase_IZh_static_info
#define FZh_static_info       PrelBase_FZh_static_info
#define DZh_static_info       PrelBase_DZh_static_info
#define AZh_static_info       PrelAddr_AZh_static_info
#define WZh_static_info       PrelAddr_WZh_static_info
#define CZh_con_info          PrelBase_CZh_con_info
#define IZh_con_info          PrelBase_IZh_con_info
#define FZh_con_info          PrelBase_FZh_con_info
#define DZh_con_info          PrelBase_DZh_con_info
#define AZh_con_info          PrelAddr_AZh_con_info
#define WZh_con_info          PrelAddr_WZh_con_info
#define W64Zh_con_info        PrelAddr_W64Zh_con_info
#define I64Zh_con_info        PrelAddr_I64Zh_con_info
#define StablePtr_static_info PrelStable_StablePtr_static_info
#define StablePtr_con_info    PrelStable_StablePtr_con_info
#define mainIO_closure        PrelMain_mainIO_closure
#define unpackCString_closure PrelPack_unpackCString_closure

#else /* INTERPRETER, I guess */

extern const StgInfoTable CZh_con_info;
extern const StgInfoTable IZh_con_info;
extern const StgInfoTable I64Zh_con_info;
extern const StgInfoTable FZh_con_info;
extern const StgInfoTable DZh_con_info;
extern const StgInfoTable AZh_con_info;
extern const StgInfoTable WZh_con_info;
extern const StgInfoTable StablePtr_con_info;

extern const StgInfoTable CZh_static_info;
extern const StgInfoTable IZh_static_info;
extern const StgInfoTable I64Zh_static_info;
extern const StgInfoTable FZh_static_info;
extern const StgInfoTable DZh_static_info;
extern const StgInfoTable AZh_static_info;
extern const StgInfoTable WZh_static_info;
extern const StgInfoTable StablePtr_static_info;

#define W64Zh_con_info        I64Zh_con_info
#define W64Zh_static_info     I64Zh_con_info

#endif

#endif /* PRELUDE_H */
