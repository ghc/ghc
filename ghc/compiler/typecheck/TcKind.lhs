\begin{code}
module TcKind (

	Kind, mkTypeKind, mkBoxedTypeKind, mkUnboxedTypeKind, mkArrowKind, 
	hasMoreBoxityInfo,	-- Kind -> Kind -> Bool
	resultKind,		-- Kind -> Kind

	TcKind, 
	newKindVar,	-- NF_TcM s (TcKind s)
	newKindVars,	-- Int -> NF_TcM s [TcKind s]
	unifyKind, 	-- TcKind s -> TcKind s -> TcM s ()
	unifyKinds, 	-- [TcKind s] -> [TcKind s] -> TcM s ()

	kindToTcKind,	-- Kind     -> TcKind s
	tcDefaultKind	-- TcKind s -> NF_TcM s Kind
  ) where

#include "HsVersions.h"

import Kind
import TcMonad

import Unique	( Unique )
import Util	( nOfThem, panic )
import Outputable
\end{code}


\begin{code}
type TcKind s = GenKind (TcRef s (TcMaybe s))
data TcMaybe s = Unbound
	       | BoundTo (TcKind s)	-- Always ArrowKind or BoxedTypeKind

newKindVar :: NF_TcM s (TcKind s)
newKindVar = tcGetUnique		`thenNF_Tc` \ uniq ->
	     tcNewMutVar Unbound	`thenNF_Tc` \ box ->
	     returnNF_Tc (VarKind uniq box)

newKindVars :: Int -> NF_TcM s [TcKind s]
newKindVars n = mapNF_Tc (\ _ -> newKindVar) (nOfThem n ())
\end{code}


Kind unification
~~~~~~~~~~~~~~~~
\begin{code}
unifyKinds :: [TcKind s] -> [TcKind s] -> TcM s ()
unifyKinds [] [] = returnTc ()
unifyKinds (k1:ks1) (k2:ks2) = unifyKind k1 k2 	`thenTc_`
			       unifyKinds ks1 ks2
unifyKinds _ _ = panic "unifyKinds: length mis-match"

unifyKind :: TcKind s		    -- Expected
	  -> TcKind s		    -- Actual
	  -> TcM s ()

unifyKind kind1 kind2
  = tcAddErrCtxtM ctxt (unify_kind kind1 kind2)
  where
    ctxt = zonkTcKind kind1	`thenNF_Tc` \ kind1' ->
	   zonkTcKind kind2	`thenNF_Tc` \ kind2' ->
	   returnNF_Tc (unifyKindCtxt kind1' kind2')
		 

-- TypeKind expected => the actual can be boxed or unboxed
unify_kind TypeKind        TypeKind        = returnTc ()
unify_kind TypeKind        BoxedTypeKind   = returnTc ()
unify_kind TypeKind        UnboxedTypeKind = returnTc ()

unify_kind BoxedTypeKind   BoxedTypeKind   = returnTc ()
unify_kind UnboxedTypeKind UnboxedTypeKind = returnTc ()

unify_kind (ArrowKind fun1 arg1)
	   (ArrowKind fun2 arg2)

  = unify_kind fun1 fun2	`thenTc_`
    unify_kind arg1 arg2

unify_kind kind1@(VarKind uniq box) kind2 = unify_var False kind1 uniq box kind2
unify_kind kind1 kind2@(VarKind uniq box) = unify_var True  kind2 uniq box kind1

unify_kind kind1 kind2
  = failWithTc (kindMisMatchErr kind1 kind2)
\end{code}

We could probably do some "shorting out" in unifyVarKind, but
I'm not convinced it would save time, and it's a little tricky to get right.

\begin{code}
unify_var swap_vars kind1 uniq1 box1 kind2
  = tcReadMutVar box1	`thenNF_Tc` \ maybe_kind1 ->
    case maybe_kind1 of
      Unbound          -> unify_unbound_var False kind1 uniq1 box1 kind2
      BoundTo TypeKind -> unify_unbound_var True  kind1 uniq1 box1 kind2
			  -- *** NB: BoundTo TypeKind is a kind of un-bound
			  --	     It can get refined to BoundTo UnboxedTypeKind or BoxedTypeKind

      BoundTo kind1' | swap_vars -> unify_kind kind2 kind1'
		     | otherwise -> unify_kind kind1' kind2
		     -- Keep them the right way round, so that
		     -- the asymettric boxed/unboxed stuff works


unify_unbound_var type_kind kind1 uniq1 box1 kind2@(VarKind uniq2 box2)
  | uniq1 == uniq2	-- Binding to self is a no-op
  = returnTc ()

  | otherwise		-- Distinct variables
  = tcReadMutVar box2	`thenNF_Tc` \ maybe_kind2 ->
    case maybe_kind2 of
	BoundTo kind2' -> unify_unbound_var type_kind kind1 uniq1 box1 kind2'
	Unbound        -> tcWriteMutVar box2 (BoundTo kind1)	`thenNF_Tc_`	
				-- No need for occurs check here
				-- Kind1 is an unbound variable, or BoundToTypeKind
		          returnTc ()

-- If the variable was originally bound to TypeKind, we succeed
-- unless the thing its bound to is an arrow.
unify_unbound_var True kind1 uniq1 box1 kind2@(ArrowKind k1 k2)
  = failWithTc (kindMisMatchErr kind1 kind2)

unify_unbound_var type_kind kind1 uniq1 box1 non_var_or_arrow_kind2
  = occur_check non_var_or_arrow_kind2			`thenTc_`
    tcWriteMutVar box1 (BoundTo non_var_or_arrow_kind2)	`thenNF_Tc_`
    returnTc ()
  where
    occur_check TypeKind  	    = returnTc ()
    occur_check UnboxedTypeKind     = returnTc ()
    occur_check BoxedTypeKind       = returnTc ()
    occur_check (ArrowKind fun arg) = occur_check fun `thenTc_` occur_check arg
    occur_check kind@(VarKind uniq' box)
	| uniq1 == uniq'
	= failWithTc (kindOccurCheck kind non_var_or_arrow_kind2)

	| otherwise	-- Different variable
	=  tcReadMutVar box		`thenNF_Tc` \ maybe_kind ->
	   case maybe_kind of
		Unbound       -> returnTc ()
		BoundTo kind' -> occur_check kind'
\end{code}

The "occurs check" is necessary to catch situation like

	type T k = k k


Kind flattening
~~~~~~~~~~~~~~~
Coercions between TcKind and Kind.  

\begin{code}
-- This strange function is forced on us by the type system
kindToTcKind :: Kind -> TcKind s
kindToTcKind TypeKind          = TypeKind
kindToTcKind BoxedTypeKind     = BoxedTypeKind
kindToTcKind UnboxedTypeKind   = UnboxedTypeKind
kindToTcKind (ArrowKind k1 k2) = ArrowKind (kindToTcKind k1) (kindToTcKind k2)


-- Default all unbound kinds to TcTypeKind, and return the
-- corresponding Kind as well.
tcDefaultKind :: TcKind s -> NF_TcM s Kind

tcDefaultKind TypeKind        = returnNF_Tc TypeKind
tcDefaultKind BoxedTypeKind   = returnNF_Tc BoxedTypeKind
tcDefaultKind UnboxedTypeKind = returnNF_Tc UnboxedTypeKind

tcDefaultKind (ArrowKind kind1 kind2)
  = tcDefaultKind kind1	`thenNF_Tc` \ k1 ->
    tcDefaultKind kind2	`thenNF_Tc` \ k2 ->
    returnNF_Tc (ArrowKind k1 k2)

	-- Here's where we "default" unbound kinds to BoxedTypeKind
tcDefaultKind (VarKind uniq box)
  = tcReadMutVar box	`thenNF_Tc` \ maybe_kind ->
    case maybe_kind of
	BoundTo TypeKind -> bind_to_boxed
	Unbound          -> bind_to_boxed
	BoundTo kind     -> tcDefaultKind kind
  where
   	-- Default unbound variables to kind BoxedTypeKind
    bind_to_boxed = tcWriteMutVar box (BoundTo BoxedTypeKind)	`thenNF_Tc_`
		    returnNF_Tc BoxedTypeKind



zonkTcKind :: TcKind s -> NF_TcM s (TcKind s)
-- Removes variables that have now been bound.
-- Mainly used just before an error message is printed,
-- so that we don't need to follow through bound variables 
-- during error message construction.

zonkTcKind TypeKind        = returnNF_Tc TypeKind
zonkTcKind BoxedTypeKind   = returnNF_Tc BoxedTypeKind
zonkTcKind UnboxedTypeKind = returnNF_Tc UnboxedTypeKind

zonkTcKind (ArrowKind kind1 kind2)
  = zonkTcKind kind1	`thenNF_Tc` \ k1 ->
    zonkTcKind kind2	`thenNF_Tc` \ k2 ->
    returnNF_Tc (ArrowKind k1 k2)

zonkTcKind kind@(VarKind uniq box)
  = tcReadMutVar box	`thenNF_Tc` \ maybe_kind ->
    case maybe_kind of
	Unbound    -> returnNF_Tc kind
	BoundTo kind' -> zonkTcKind kind'
\end{code}


Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
unifyKindCtxt kind1 kind2
  = vcat [ptext SLIT("Expected:") <+> ppr kind1, 
	  ptext SLIT("Found:   ") <+> ppr kind2]

kindOccurCheck kind1 kind2
  = hang (ptext SLIT("Cannot construct the infinite kind:")) 4
	(sep [ppr kind1, equals, ppr kind1, ptext SLIT("(\"occurs check\")")])

kindMisMatchErr kind1 kind2
 = hang (ptext SLIT("Couldn't match the kind")) 4
	(sep [ppr kind1,
	      ptext SLIT("against"),
	      ppr kind2]
	)
\end{code}
