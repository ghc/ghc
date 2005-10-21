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

PRELUDE_CLOSURE(GHCziBase_True_closure);
PRELUDE_CLOSURE(GHCziBase_False_closure);
PRELUDE_CLOSURE(GHCziPack_unpackCString_closure);
PRELUDE_CLOSURE(GHCziWeak_runFinalizzerBatch_closure);

#ifdef IN_STG_CODE
extern W_ ZCMain_main_closure[];
#else
extern StgClosure ZCMain_main_closure;
#endif

PRELUDE_CLOSURE(GHCziIOBase_stackOverflow_closure);
PRELUDE_CLOSURE(GHCziIOBase_heapOverflow_closure);
PRELUDE_CLOSURE(GHCziIOBase_BlockedOnDeadMVar_closure);
PRELUDE_CLOSURE(GHCziIOBase_BlockedIndefinitely_closure);
PRELUDE_CLOSURE(GHCziIOBase_NonTermination_closure);

PRELUDE_INFO(GHCziBase_Czh_static_info);
PRELUDE_INFO(GHCziBase_Izh_static_info);
PRELUDE_INFO(GHCziFloat_Fzh_static_info);
PRELUDE_INFO(GHCziFloat_Dzh_static_info);
PRELUDE_INFO(Addr_Azh_static_info);
PRELUDE_INFO(GHCziPtr_Ptr_static_info);
PRELUDE_INFO(GHCziPtr_FunPtr_static_info);
PRELUDE_INFO(GHCziInt_I8zh_static_info);
PRELUDE_INFO(GHCziInt_I16zh_static_info);
PRELUDE_INFO(GHCziInt_I32zh_static_info);
PRELUDE_INFO(GHCziInt_I64zh_static_info);
PRELUDE_INFO(GHCziWord_Wzh_static_info);
PRELUDE_INFO(GHCziWord_W8zh_static_info);
PRELUDE_INFO(GHCziWord_W16zh_static_info);
PRELUDE_INFO(GHCziWord_W32zh_static_info);
PRELUDE_INFO(GHCziWord_W64zh_static_info);
PRELUDE_INFO(GHCziBase_Czh_con_info);
PRELUDE_INFO(GHCziBase_Izh_con_info);
PRELUDE_INFO(GHCziFloat_Fzh_con_info);
PRELUDE_INFO(GHCziFloat_Dzh_con_info);
PRELUDE_INFO(GHCziPtr_Ptr_con_info);
PRELUDE_INFO(GHCziPtr_FunPtr_con_info);
PRELUDE_INFO(Addr_Azh_con_info);
PRELUDE_INFO(GHCziWord_Wzh_con_info);
PRELUDE_INFO(GHCziInt_I8zh_con_info);
PRELUDE_INFO(GHCziInt_I16zh_con_info);
PRELUDE_INFO(GHCziInt_I32zh_con_info);
PRELUDE_INFO(GHCziInt_I64zh_con_info);
PRELUDE_INFO(GHCziWord_W8zh_con_info);
PRELUDE_INFO(GHCziWord_W16zh_con_info);
PRELUDE_INFO(GHCziWord_W32zh_con_info);
PRELUDE_INFO(GHCziWord_W64zh_con_info);
PRELUDE_INFO(GHCziStable_StablePtr_static_info);
PRELUDE_INFO(GHCziStable_StablePtr_con_info);

#define True_closure              (&GHCziBase_True_closure)
#define False_closure             (&GHCziBase_False_closure)
#define unpackCString_closure     (&GHCziPack_unpackCString_closure)
#define runFinalizerBatch_closure (&GHCziWeak_runFinalizzerBatch_closure)
#define mainIO_closure            (&ZCMain_main_closure)

#define stackOverflow_closure     (&GHCziIOBase_stackOverflow_closure)
#define heapOverflow_closure      (&GHCziIOBase_heapOverflow_closure)
#define BlockedOnDeadMVar_closure (&GHCziIOBase_BlockedOnDeadMVar_closure)
#define BlockedIndefinitely_closure (&GHCziIOBase_BlockedIndefinitely_closure)
#define NonTermination_closure    (&GHCziIOBase_NonTermination_closure)

#define Czh_static_info           (&GHCziBase_Czh_static_info)
#define Fzh_static_info           (&GHCziFloat_Fzh_static_info)
#define Dzh_static_info           (&GHCziFloat_Dzh_static_info)
#define Azh_static_info           (&Addr_Azh_static_info)
#define Izh_static_info           (&GHCziBase_Izh_static_info)
#define I8zh_static_info          (&GHCziInt_I8zh_static_info)
#define I16zh_static_info         (&GHCziInt_I16zh_static_info)
#define I32zh_static_info         (&GHCziInt_I32zh_static_info)
#define I64zh_static_info         (&GHCziInt_I64zh_static_info)
#define Wzh_static_info           (&GHCziWord_Wzh_static_info)
#define W8zh_static_info          (&GHCziWord_W8zh_static_info)
#define W16zh_static_info         (&GHCziWord_W16zh_static_info)
#define W32zh_static_info         (&GHCziWord_W32zh_static_info)
#define W64zh_static_info         (&GHCziWord_W64zh_static_info)
#define Ptr_static_info           (&GHCziPtr_Ptr_static_info)
#define FunPtr_static_info        (&GHCziPtr_FunPtr_static_info)
#define Czh_con_info              (&GHCziBase_Czh_con_info)
#define Izh_con_info              (&GHCziBase_Izh_con_info)
#define Fzh_con_info              (&GHCziFloat_Fzh_con_info)
#define Dzh_con_info              (&GHCziFloat_Dzh_con_info)
#define Azh_con_info              (&Addr_Azh_con_info)
#define Wzh_con_info              (&GHCziWord_Wzh_con_info)
#define W8zh_con_info             (&GHCziWord_W8zh_con_info)
#define W16zh_con_info            (&GHCziWord_W16zh_con_info)
#define W32zh_con_info            (&GHCziWord_W32zh_con_info)
#define W64zh_con_info            (&GHCziWord_W64zh_con_info)
#define I8zh_con_info             (&GHCziInt_I8zh_con_info)
#define I16zh_con_info            (&GHCziInt_I16zh_con_info)
#define I32zh_con_info            (&GHCziInt_I32zh_con_info)
#define I64zh_con_info            (&GHCziInt_I64zh_con_info)
#define I64zh_con_info            (&GHCziInt_I64zh_con_info)
#define Ptr_con_info              (&GHCziPtr_Ptr_con_info)
#define FunPtr_con_info           (&GHCziPtr_FunPtr_con_info)
#define StablePtr_static_info     (&GHCziStable_StablePtr_static_info)
#define StablePtr_con_info        (&GHCziStable_StablePtr_con_info)

#endif /* PRELUDE_H */
