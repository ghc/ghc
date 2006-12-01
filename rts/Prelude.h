/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRELUDE_H
#define PRELUDE_H

/* These definitions are required by the RTS .cmm files too, so we
 * need declarations that we can #include into the generated .hc files.
 */
#if IN_STG_CODE
#define PRELUDE_INFO(i)       extern W_(i)[]
#define PRELUDE_CLOSURE(i)    extern W_(i)[]
#else
#define PRELUDE_INFO(i)       extern DLL_IMPORT const StgInfoTable i
#define PRELUDE_CLOSURE(i)    extern DLL_IMPORT StgClosure i
#endif

/* Define canonical names so we can abstract away from the actual
 * modules these names are defined in.
 */

PRELUDE_CLOSURE(base_GHCziBase_True_closure);
PRELUDE_CLOSURE(base_GHCziBase_False_closure);
PRELUDE_CLOSURE(base_GHCziPack_unpackCString_closure);
PRELUDE_CLOSURE(base_GHCziWeak_runFinalizzerBatch_closure);

#ifdef IN_STG_CODE
extern W_ ZCMain_main_closure[];
#else
extern StgClosure ZCMain_main_closure;
#endif

PRELUDE_CLOSURE(base_GHCziIOBase_stackOverflow_closure);
PRELUDE_CLOSURE(base_GHCziIOBase_heapOverflow_closure);
PRELUDE_CLOSURE(base_GHCziIOBase_BlockedOnDeadMVar_closure);
PRELUDE_CLOSURE(base_GHCziIOBase_BlockedIndefinitely_closure);
PRELUDE_CLOSURE(base_GHCziIOBase_NonTermination_closure);
PRELUDE_CLOSURE(base_GHCziIOBase_NestedAtomically_closure);

PRELUDE_CLOSURE(base_GHCziConc_ensureIOManagerIsRunning_closure);

PRELUDE_INFO(base_GHCziBase_Czh_static_info);
PRELUDE_INFO(base_GHCziBase_Izh_static_info);
PRELUDE_INFO(base_GHCziFloat_Fzh_static_info);
PRELUDE_INFO(base_GHCziFloat_Dzh_static_info);
PRELUDE_INFO(base_Addr_Azh_static_info);
PRELUDE_INFO(base_GHCziPtr_Ptr_static_info);
PRELUDE_INFO(base_GHCziPtr_FunPtr_static_info);
PRELUDE_INFO(base_GHCziInt_I8zh_static_info);
PRELUDE_INFO(base_GHCziInt_I16zh_static_info);
PRELUDE_INFO(base_GHCziInt_I32zh_static_info);
PRELUDE_INFO(base_GHCziInt_I64zh_static_info);
PRELUDE_INFO(base_GHCziWord_Wzh_static_info);
PRELUDE_INFO(base_GHCziWord_W8zh_static_info);
PRELUDE_INFO(base_GHCziWord_W16zh_static_info);
PRELUDE_INFO(base_GHCziWord_W32zh_static_info);
PRELUDE_INFO(base_GHCziWord_W64zh_static_info);
PRELUDE_INFO(base_GHCziBase_Czh_con_info);
PRELUDE_INFO(base_GHCziBase_Izh_con_info);
PRELUDE_INFO(base_GHCziFloat_Fzh_con_info);
PRELUDE_INFO(base_GHCziFloat_Dzh_con_info);
PRELUDE_INFO(base_GHCziPtr_Ptr_con_info);
PRELUDE_INFO(base_GHCziPtr_FunPtr_con_info);
PRELUDE_INFO(base_Addr_Azh_con_info);
PRELUDE_INFO(base_GHCziWord_Wzh_con_info);
PRELUDE_INFO(base_GHCziInt_I8zh_con_info);
PRELUDE_INFO(base_GHCziInt_I16zh_con_info);
PRELUDE_INFO(base_GHCziInt_I32zh_con_info);
PRELUDE_INFO(base_GHCziInt_I64zh_con_info);
PRELUDE_INFO(base_GHCziWord_W8zh_con_info);
PRELUDE_INFO(base_GHCziWord_W16zh_con_info);
PRELUDE_INFO(base_GHCziWord_W32zh_con_info);
PRELUDE_INFO(base_GHCziWord_W64zh_con_info);
PRELUDE_INFO(base_GHCziStable_StablePtr_static_info);
PRELUDE_INFO(base_GHCziStable_StablePtr_con_info);

#define True_closure              (&base_GHCziBase_True_closure)
#define False_closure             (&base_GHCziBase_False_closure)
#define unpackCString_closure     (&base_GHCziPack_unpackCString_closure)
#define runFinalizerBatch_closure (&base_GHCziWeak_runFinalizzerBatch_closure)
#define mainIO_closure            (&ZCMain_main_closure)

#define stackOverflow_closure     (&base_GHCziIOBase_stackOverflow_closure)
#define heapOverflow_closure      (&base_GHCziIOBase_heapOverflow_closure)
#define BlockedOnDeadMVar_closure (&base_GHCziIOBase_BlockedOnDeadMVar_closure)
#define BlockedIndefinitely_closure (&base_GHCziIOBase_BlockedIndefinitely_closure)
#define NonTermination_closure    (&base_GHCziIOBase_NonTermination_closure)
#define NestedAtomically_closure  (&base_GHCziIOBase_NestedAtomically_closure)

#define Czh_static_info           (&base_GHCziBase_Czh_static_info)
#define Fzh_static_info           (&base_GHCziFloat_Fzh_static_info)
#define Dzh_static_info           (&base_GHCziFloat_Dzh_static_info)
#define Azh_static_info           (&base_Addr_Azh_static_info)
#define Izh_static_info           (&base_GHCziBase_Izh_static_info)
#define I8zh_static_info          (&base_GHCziInt_I8zh_static_info)
#define I16zh_static_info         (&base_GHCziInt_I16zh_static_info)
#define I32zh_static_info         (&base_GHCziInt_I32zh_static_info)
#define I64zh_static_info         (&base_GHCziInt_I64zh_static_info)
#define Wzh_static_info           (&base_GHCziWord_Wzh_static_info)
#define W8zh_static_info          (&base_GHCziWord_W8zh_static_info)
#define W16zh_static_info         (&base_GHCziWord_W16zh_static_info)
#define W32zh_static_info         (&base_GHCziWord_W32zh_static_info)
#define W64zh_static_info         (&base_GHCziWord_W64zh_static_info)
#define Ptr_static_info           (&base_GHCziPtr_Ptr_static_info)
#define FunPtr_static_info        (&base_GHCziPtr_FunPtr_static_info)
#define Czh_con_info              (&base_GHCziBase_Czh_con_info)
#define Izh_con_info              (&base_GHCziBase_Izh_con_info)
#define Fzh_con_info              (&base_GHCziFloat_Fzh_con_info)
#define Dzh_con_info              (&base_GHCziFloat_Dzh_con_info)
#define Azh_con_info              (&base_Addr_Azh_con_info)
#define Wzh_con_info              (&base_GHCziWord_Wzh_con_info)
#define W8zh_con_info             (&base_GHCziWord_W8zh_con_info)
#define W16zh_con_info            (&base_GHCziWord_W16zh_con_info)
#define W32zh_con_info            (&base_GHCziWord_W32zh_con_info)
#define W64zh_con_info            (&base_GHCziWord_W64zh_con_info)
#define I8zh_con_info             (&base_GHCziInt_I8zh_con_info)
#define I16zh_con_info            (&base_GHCziInt_I16zh_con_info)
#define I32zh_con_info            (&base_GHCziInt_I32zh_con_info)
#define I64zh_con_info            (&base_GHCziInt_I64zh_con_info)
#define I64zh_con_info            (&base_GHCziInt_I64zh_con_info)
#define Ptr_con_info              (&base_GHCziPtr_Ptr_con_info)
#define FunPtr_con_info           (&base_GHCziPtr_FunPtr_con_info)
#define StablePtr_static_info     (&base_GHCziStable_StablePtr_static_info)
#define StablePtr_con_info        (&base_GHCziStable_StablePtr_con_info)

#endif /* PRELUDE_H */
