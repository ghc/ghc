Stuff the Ugenny things show to the parser.

\begin{code}
#include "HsVersions.h"

module UgenAll (
	-- re-exported Prelude stuff
	returnUgn, thenUgn,

	-- stuff defined in utils module
#if (! defined(REALLY_HASKELL_1_3)) || PATRICK_FIXES_MODULE_DOTDOT_THING
	EXP_MODULE(UgenUtil) ,

	-- re-exported ugen-generated stuff
	EXP_MODULE(U_binding) ,
	EXP_MODULE(U_constr) ,
	EXP_MODULE(U_entidt) ,
	EXP_MODULE(U_list) ,
	EXP_MODULE(U_literal) ,
	EXP_MODULE(U_maybe) ,
	EXP_MODULE(U_either) ,
	EXP_MODULE(U_pbinding) ,
	EXP_MODULE(U_qid) ,
	EXP_MODULE(U_tree) ,
	EXP_MODULE(U_ttype)
#else
	SYN_IE(ParseTree),
	SYN_IE(U_VOID_STAR),
	U_binding (..),
	U_constr (..),
	U_either (..),
	U_entidt (..),
	SYN_IE(U_hstring),
	U_list (..),
	U_literal (..),
	SYN_IE(U_long),
	U_maybe (..),
	SYN_IE(U_numId),
	U_pbinding (..),
	U_qid (..),
	SYN_IE(U_stringId),
	U_tree (..),
	U_ttype (..),
	SYN_IE(UgnM),
	getSrcFileUgn,
	getSrcLocUgn,
	getSrcModUgn,
	initUgn,
	ioToUgnM,
	mkSrcLocUgn,
	rdU_VOID_STAR,
	rdU_binding,
	rdU_constr,
	rdU_either,
	rdU_entidt,
	rdU_hstring,
	rdU_list,
	rdU_literal,
	rdU_long,
	rdU_maybe,
	rdU_numId,
	rdU_pbinding,
	rdU_qid,
	rdU_stringId,
	rdU_tree,
	rdU_ttype,
	setSrcFileUgn,
	setSrcModUgn
#endif
    ) where

import PreludeGlaST

IMP_Ubiq(){-uitous-}

-- friends:
import U_binding
import U_constr
import U_entidt
import U_list
import U_literal
import U_maybe
import U_either
import U_pbinding
import U_qid
import U_tree
import U_ttype

import UgenUtil
\end{code}
