\begin{code}
#include "HsVersions.h"

module TcKind (

	Kind, mkTypeKind, mkBoxedTypeKind, mkUnboxedTypeKind, mkArrowKind, 
	hasMoreBoxityInfo,	-- Kind -> Kind -> Bool
	resultKind,		-- Kind -> Kind

	TcKind, mkTcTypeKind, mkTcArrowKind, mkTcVarKind,
	newKindVar,	-- NF_TcM s (TcKind s)
	newKindVars,	-- Int -> NF_TcM s [TcKind s]
	unifyKind, 	-- TcKind s -> TcKind s -> TcM s ()

	kindToTcKind,	-- Kind     -> TcKind s
	tcDefaultKind	-- TcKind s -> NF_TcM s Kind
  ) where

IMP_Ubiq(){-uitous-}

import Kind
import TcMonad

import Unique	( Unique, pprUnique10 )
import Pretty
import Util	( nOfThem )
\end{code}


\begin{code}
data TcKind s		-- Used for kind inference
  = TcTypeKind
  | TcArrowKind (TcKind s) (TcKind s)
  | TcVarKind Unique (MutableVar s (Maybe (TcKind s)))

mkTcTypeKind  = TcTypeKind
mkTcArrowKind = TcArrowKind
mkTcVarKind   = TcVarKind

newKindVar :: NF_TcM s (TcKind s)
newKindVar = tcGetUnique		`thenNF_Tc` \ uniq ->
	     tcNewMutVar Nothing	`thenNF_Tc` \ box ->
	     returnNF_Tc (TcVarKind uniq box)

newKindVars :: Int -> NF_TcM s [TcKind s]
newKindVars n = mapNF_Tc (\ _ -> newKindVar) (nOfThem n ())
\end{code}


Kind unification
~~~~~~~~~~~~~~~~
\begin{code}
unifyKind :: TcKind s -> TcKind s -> TcM s ()
unifyKind kind1 kind2
  = tcAddErrCtxtM ctxt (unify_kind kind1 kind2)
  where
    ctxt = zonkTcKind kind1	`thenNF_Tc` \ kind1' ->
	   zonkTcKind kind2	`thenNF_Tc` \ kind2' ->
	   returnNF_Tc (unifyKindCtxt kind1' kind2')
		 

unify_kind TcTypeKind TcTypeKind = returnTc ()

unify_kind (TcArrowKind fun1 arg1)
	   (TcArrowKind fun2 arg2)

  = unify_kind fun1 fun2	`thenTc_`
    unify_kind arg1 arg2

unify_kind (TcVarKind uniq box) kind = unify_var uniq box kind
unify_kind kind (TcVarKind uniq box) = unify_var uniq box kind

unify_kind kind1 kind2
  = failTc (kindMisMatchErr kind1 kind2)
\end{code}

We could probably do some "shorting out" in unifyVarKind, but
I'm not convinced it would save time, and it's a little tricky to get right.

\begin{code}
unify_var uniq1 box1 kind2
  = tcReadMutVar box1	`thenNF_Tc` \ maybe_kind1 ->
    case maybe_kind1 of
      Just kind1 -> unify_kind kind1 kind2
      Nothing    -> unify_unbound_var uniq1 box1 kind2

unify_unbound_var uniq1 box1 kind2@(TcVarKind uniq2 box2)
  | uniq1 == uniq2	-- Binding to self is a no-op
  = returnTc ()

  | otherwise		-- Distinct variables
  = tcReadMutVar box2	`thenNF_Tc` \ maybe_kind2 ->
    case maybe_kind2 of
	Just kind2' -> unify_unbound_var uniq1 box1 kind2'
	Nothing     -> tcWriteMutVar box1 (Just kind2)	`thenNF_Tc_`	
				-- No need for occurs check here
		       returnTc ()

unify_unbound_var uniq1 box1 non_var_kind2
  = occur_check non_var_kind2			`thenTc_`
    tcWriteMutVar box1 (Just non_var_kind2)	`thenNF_Tc_`
    returnTc ()
  where
    occur_check TcTypeKind  	      = returnTc ()
    occur_check (TcArrowKind fun arg) = occur_check fun `thenTc_` occur_check arg
    occur_check kind1@(TcVarKind uniq' box)
	| uniq1 == uniq'
	= failTc (kindOccurCheck kind1 non_var_kind2)

	| otherwise	-- Different variable
	=  tcReadMutVar box		`thenNF_Tc` \ maybe_kind ->
	   case maybe_kind of
		Nothing   -> returnTc ()
		Just kind -> occur_check kind
\end{code}

The "occurs check" is necessary to catch situation like

	type T k = k k


Kind flattening
~~~~~~~~~~~~~~~
Coercions between TcKind and Kind

\begin{code}
kindToTcKind :: Kind -> TcKind s
kindToTcKind TypeKind          = TcTypeKind
kindToTcKind BoxedTypeKind     = TcTypeKind
kindToTcKind UnboxedTypeKind   = TcTypeKind
kindToTcKind (ArrowKind k1 k2) = TcArrowKind (kindToTcKind k1) (kindToTcKind k2)


-- Default all unbound kinds to TcTypeKind, and return the
-- corresponding Kind as well.
tcDefaultKind :: TcKind s -> NF_TcM s Kind

tcDefaultKind TcTypeKind
  = returnNF_Tc BoxedTypeKind

tcDefaultKind (TcArrowKind kind1 kind2)
  = tcDefaultKind kind1	`thenNF_Tc` \ k1 ->
    tcDefaultKind kind2	`thenNF_Tc` \ k2 ->
    returnNF_Tc (ArrowKind k1 k2)

	-- Here's where we "default" unbound kinds to BoxedTypeKind
tcDefaultKind (TcVarKind uniq box)
  = tcReadMutVar box	`thenNF_Tc` \ maybe_kind ->
    case maybe_kind of
	Just kind -> tcDefaultKind kind

	Nothing   -> 	-- Default unbound variables to kind Type
		     tcWriteMutVar box (Just TcTypeKind)	`thenNF_Tc_`
		     returnNF_Tc BoxedTypeKind

zonkTcKind :: TcKind s -> NF_TcM s (TcKind s)
-- Removes variables that have now been bound.
-- Mainly used just before an error message is printed,
-- so that we don't need to follow through bound variables 
-- during error message construction.

zonkTcKind TcTypeKind = returnNF_Tc TcTypeKind

zonkTcKind (TcArrowKind kind1 kind2)
  = zonkTcKind kind1	`thenNF_Tc` \ k1 ->
    zonkTcKind kind2	`thenNF_Tc` \ k2 ->
    returnNF_Tc (TcArrowKind k1 k2)

zonkTcKind kind@(TcVarKind uniq box)
  = tcReadMutVar box	`thenNF_Tc` \ maybe_kind ->
    case maybe_kind of
	Nothing    -> returnNF_Tc kind
	Just kind' -> zonkTcKind kind'
\end{code}


\begin{code}
instance Outputable (TcKind s) where
  ppr sty kind = ppr_kind sty kind

ppr_kind sty TcTypeKind 
  = ppChar '*'
ppr_kind sty (TcArrowKind kind1 kind2) 
  = ppSep [ppr_parend sty kind1, ppPStr SLIT("->"), ppr_kind sty kind2]
ppr_kind sty (TcVarKind uniq box) 
  = ppBesides [ppChar 'k', pprUnique10 uniq]

ppr_parend sty kind@(TcArrowKind _ _) = ppBesides [ppChar '(', ppr_kind sty kind, ppChar ')']
ppr_parend sty other_kind	      = ppr_kind sty other_kind
\end{code}



Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
unifyKindCtxt kind1 kind2 sty
  = ppHang (ppPStr SLIT("When unifying two kinds")) 4
	   (ppSep [ppr sty kind1, ppPStr SLIT("and"), ppr sty kind2])

kindOccurCheck kind1 kind2 sty
  = ppHang (ppPStr SLIT("Cannot construct the infinite kind:")) 4
	(ppSep [ppBesides [ppChar '`', ppr sty kind1, ppChar '\''],
		ppChar '=',
		ppBesides [ppChar '`', ppr sty kind1, ppChar '\''],
		ppPStr SLIT("(\"occurs check\")")])

kindMisMatchErr kind1 kind2 sty
 = ppHang (ppPStr SLIT("Couldn't match the kind")) 4
	(ppSep [ppBesides [ppChar '`', ppr sty kind1, ppChar '\''],
		ppPStr SLIT("against"),
		ppBesides [ppChar '`', ppr sty kind2, ppChar '\'']
	])
\end{code}
