%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module CoreUtils (
	-- Construction
	mkNote, mkInlineMe, mkSCC, mkCoerce,
	bindNonRec, mkIfThenElse, mkAltExpr,
        mkPiType,

	-- Taking expressions apart
	findDefault, findAlt, hasDefault,

	-- Properties of expressions
	exprType, coreAltsType, 
	exprIsBottom, exprIsDupable, exprIsTrivial, exprIsCheap, 
	exprIsValue,exprOkForSpeculation, exprIsBig, 
	exprIsConApp_maybe, exprIsAtom,
	idAppIsBottom, idAppIsCheap,
	exprArity, 

	-- Expr transformation
	etaReduce, etaExpand,
	exprArity, exprEtaExpandArity, 

	-- Size
	coreBindsSize,

	-- Hashing
	hashExpr,

	-- Equality
	cheapEqExpr, eqExpr, applyTypeToArgs
    ) where

#include "HsVersions.h"


import GlaExts		-- For `xori` 

import CoreSyn
import CoreFVs		( exprFreeVars )
import PprCore		( pprCoreExpr )
import Var		( Var, isId, isTyVar )
import VarSet
import VarEnv
import Name		( hashName )
import Literal		( hashLiteral, literalType, litIsDupable )
import DataCon		( DataCon, dataConRepArity )
import PrimOp		( primOpOkForSpeculation, primOpIsCheap )
import Id		( Id, idType, globalIdDetails, idStrictness, idLBVarInfo, 
			  mkWildId, idArity, idName, idUnfolding, idInfo, isOneShotLambda,
			  isDataConId_maybe, mkSysLocal, hasNoBinding
			)
import IdInfo		( LBVarInfo(..),  
			  GlobalIdDetails(..),
			  megaSeqIdInfo )
import Demand		( appIsBottom )
import Type		( Type, mkFunTy, mkForAllTy, splitFunTy_maybe, 
			  applyTys, isUnLiftedType, seqType, mkUTy, mkTyVarTy,
			  splitForAllTy_maybe, isForAllTy, eqType
			)
import TysWiredIn	( boolTy, trueDataCon, falseDataCon )
import CostCentre	( CostCentre )
import UniqSupply	( UniqSupply, splitUniqSupply, uniqFromSupply )
import Outputable
import TysPrim		( alphaTy )	-- Debugging only
\end{code}


%************************************************************************
%*									*
\subsection{Find the type of a Core atom/expression}
%*									*
%************************************************************************

\begin{code}
exprType :: CoreExpr -> Type

exprType (Var var)		= idType var
exprType (Lit lit)		= literalType lit
exprType (Let _ body)	   	= exprType body
exprType (Case _ _ alts)        = coreAltsType alts
exprType (Note (Coerce ty _) e) = ty  -- **! should take usage from e
exprType (Note other_note e)    = exprType e
exprType (Lam binder expr)      = mkPiType binder (exprType expr)
exprType e@(App _ _)
  = case collectArgs e of
	(fun, args) -> applyTypeToArgs e (exprType fun) args

exprType other = pprTrace "exprType" (pprCoreExpr other) alphaTy

coreAltsType :: [CoreAlt] -> Type
coreAltsType ((_,_,rhs) : _) = exprType rhs
\end{code}

@mkPiType@ makes a (->) type or a forall type, depending on whether
it is given a type variable or a term variable.  We cleverly use the
lbvarinfo field to figure out the right annotation for the arrove in
case of a term variable.

\begin{code}
mkPiType :: Var -> Type -> Type		-- The more polymorphic version doesn't work...
mkPiType v ty | isId v    = (case idLBVarInfo v of
                               LBVarInfo u -> mkUTy u
                               otherwise   -> id) $
                            mkFunTy (idType v) ty
	      | isTyVar v = mkForAllTy v ty
\end{code}

\begin{code}
-- The first argument is just for debugging
applyTypeToArgs :: CoreExpr -> Type -> [CoreExpr] -> Type
applyTypeToArgs e op_ty [] = op_ty

applyTypeToArgs e op_ty (Type ty : args)
  =	-- Accumulate type arguments so we can instantiate all at once
    applyTypeToArgs e (applyTys op_ty tys) rest_args
  where
    (tys, rest_args)        = go [ty] args
    go tys (Type ty : args) = go (ty:tys) args
    go tys rest_args	    = (reverse tys, rest_args)

applyTypeToArgs e op_ty (other_arg : args)
  = case (splitFunTy_maybe op_ty) of
	Just (_, res_ty) -> applyTypeToArgs e res_ty args
	Nothing -> pprPanic "applyTypeToArgs" (pprCoreExpr e)
\end{code}



%************************************************************************
%*									*
\subsection{Attaching notes}
%*									*
%************************************************************************

mkNote removes redundant coercions, and SCCs where possible

\begin{code}
mkNote :: Note -> CoreExpr -> CoreExpr
mkNote (Coerce to_ty from_ty) expr = mkCoerce to_ty from_ty expr
mkNote (SCC cc)	expr		   = mkSCC cc expr
mkNote InlineMe expr		   = mkInlineMe expr
mkNote note     expr		   = Note note expr

-- Slide InlineCall in around the function
--	No longer necessary I think (SLPJ Apr 99)
-- mkNote InlineCall (App f a) = App (mkNote InlineCall f) a
-- mkNote InlineCall (Var v)   = Note InlineCall (Var v)
-- mkNote InlineCall expr      = expr
\end{code}

Drop trivial InlineMe's.  This is somewhat important, because if we have an unfolding
that looks like	(Note InlineMe (Var v)), the InlineMe doesn't go away because it may
not be *applied* to anything.

We don't use exprIsTrivial here, though, because we sometimes generate worker/wrapper
bindings like
	fw = ...
	f  = inline_me (coerce t fw)
As usual, the inline_me prevents the worker from getting inlined back into the wrapper.
We want the split, so that the coerces can cancel at the call site.  

However, we can get left with tiresome type applications.  Notably, consider
	f = /\ a -> let t = e in (t, w)
Then lifting the let out of the big lambda gives
	t' = /\a -> e
	f = /\ a -> let t = inline_me (t' a) in (t, w)
The inline_me is to stop the simplifier inlining t' right back
into t's RHS.  In the next phase we'll substitute for t (since
its rhs is trivial) and *then* we could get rid of the inline_me.
But it hardly seems worth it, so I don't bother.

\begin{code}
mkInlineMe (Var v) = Var v
mkInlineMe e	   = Note InlineMe e
\end{code}



\begin{code}
mkCoerce :: Type -> Type -> CoreExpr -> CoreExpr

mkCoerce to_ty from_ty (Note (Coerce to_ty2 from_ty2) expr)
  = ASSERT( from_ty `eqType` to_ty2 )
    mkCoerce to_ty from_ty2 expr

mkCoerce to_ty from_ty expr
  | to_ty `eqType` from_ty = expr
  | otherwise	  	   = ASSERT( from_ty `eqType` exprType expr )
			     Note (Coerce to_ty from_ty) expr
\end{code}

\begin{code}
mkSCC :: CostCentre -> Expr b -> Expr b
	-- Note: Nested SCC's *are* preserved for the benefit of
	--       cost centre stack profiling
mkSCC cc (Lit lit)  	    = Lit lit
mkSCC cc (Lam x e)  	    = Lam x (mkSCC cc e)  -- Move _scc_ inside lambda
mkSCC cc (Note (SCC cc') e) = Note (SCC cc) (Note (SCC cc') e)
mkSCC cc (Note n e) 	    = Note n (mkSCC cc e) -- Move _scc_ inside notes
mkSCC cc expr	    	    = Note (SCC cc) expr
\end{code}


%************************************************************************
%*									*
\subsection{Other expression construction}
%*									*
%************************************************************************

\begin{code}
bindNonRec :: Id -> CoreExpr -> CoreExpr -> CoreExpr
-- (bindNonRec x r b) produces either
--	let x = r in b
-- or
--	case r of x { _DEFAULT_ -> b }
--
-- depending on whether x is unlifted or not
-- It's used by the desugarer to avoid building bindings
-- that give Core Lint a heart attack.  Actually the simplifier
-- deals with them perfectly well.
bindNonRec bndr rhs body 
  | isUnLiftedType (idType bndr) = Case rhs bndr [(DEFAULT,[],body)]
  | otherwise			 = Let (NonRec bndr rhs) body
\end{code}

\begin{code}
mkAltExpr :: AltCon -> [CoreBndr] -> [Type] -> CoreExpr
	-- This guy constructs the value that the scrutinee must have
	-- when you are in one particular branch of a case
mkAltExpr (DataAlt con) args inst_tys
  = mkConApp con (map Type inst_tys ++ map varToCoreExpr args)
mkAltExpr (LitAlt lit) [] []
  = Lit lit

mkIfThenElse :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
mkIfThenElse guard then_expr else_expr
  = Case guard (mkWildId boolTy) 
	 [ (DataAlt trueDataCon,  [], then_expr),
	   (DataAlt falseDataCon, [], else_expr) ]
\end{code}


%************************************************************************
%*									*
\subsection{Taking expressions apart}
%*									*
%************************************************************************

The default alternative must be first, if it exists at all.
This makes it easy to find, though it makes matching marginally harder.

\begin{code}
hasDefault :: [CoreAlt] -> Bool
hasDefault ((DEFAULT,_,_) : alts) = True
hasDefault _			  = False

findDefault :: [CoreAlt] -> ([CoreAlt], Maybe CoreExpr)
findDefault ((DEFAULT,args,rhs) : alts) = ASSERT( null args ) (alts, Just rhs)
findDefault alts			= 		      (alts, Nothing)

findAlt :: AltCon -> [CoreAlt] -> CoreAlt
findAlt con alts
  = case alts of
	(deflt@(DEFAULT,_,_):alts) -> go alts deflt
	other			   -> go alts panic_deflt

  where
    panic_deflt = pprPanic "Missing alternative" (ppr con $$ vcat (map ppr alts))

    go []	 	       deflt 		   = deflt
    go (alt@(con1,_,_) : alts) deflt | con == con1 = alt
				     | otherwise   = ASSERT( not (con1 == DEFAULT) )
						     go alts deflt
\end{code}


%************************************************************************
%*									*
\subsection{Figuring out things about expressions}
%*									*
%************************************************************************

@exprIsTrivial@ is true of expressions we are unconditionally happy to
		duplicate; simple variables and constants, and type
		applications.  Note that primop Ids aren't considered
		trivial unless 

@exprIsBottom@	is true of expressions that are guaranteed to diverge


\begin{code}
exprIsTrivial (Var v)
  | hasNoBinding v		       = idArity v == 0
	-- WAS: | Just op <- isPrimOpId_maybe v      = primOpIsDupable op
	-- The idea here is that a constructor worker, like $wJust, is
	-- really short for (\x -> $wJust x), becuase $wJust has no binding.
	-- So it should be treated like a lambda.
	-- Ditto unsaturated primops.
	-- This came up when dealing with eta expansion/reduction for
	-- 	x = $wJust
	-- Here we want to eta-expand.  This looks like an optimisation,
	-- but it's important (albeit tiresome) that CoreSat doesn't increase 
	-- anything's arity
  | otherwise                          = True
exprIsTrivial (Type _)	      	       = True
exprIsTrivial (Lit lit)       	       = True
exprIsTrivial (App e arg)     	       = not (isRuntimeArg arg) && exprIsTrivial e
exprIsTrivial (Note _ e)      	       = exprIsTrivial e
exprIsTrivial (Lam b body)             = not (isRuntimeVar b) && exprIsTrivial body
exprIsTrivial other	      	       = False

exprIsAtom :: CoreExpr -> Bool
-- Used to decide whether to let-binding an STG argument
-- when compiling to ILX => type applications are not allowed
exprIsAtom (Var v)    = True	-- primOpIsDupable?
exprIsAtom (Lit lit)  = True
exprIsAtom (Type ty)  = True
exprIsAtom (Note (SCC _) e) = False
exprIsAtom (Note _ e) = exprIsAtom e
exprIsAtom other      = False
\end{code}


@exprIsDupable@	is true of expressions that can be duplicated at a modest
		cost in code size.  This will only happen in different case
		branches, so there's no issue about duplicating work.

		That is, exprIsDupable returns True of (f x) even if
		f is very very expensive to call.

		Its only purpose is to avoid fruitless let-binding
		and then inlining of case join points


\begin{code}
exprIsDupable (Type _)	     	= True
exprIsDupable (Var v)	     	= True
exprIsDupable (Lit lit)      	= litIsDupable lit
exprIsDupable (Note InlineMe e) = True
exprIsDupable (Note _ e)        = exprIsDupable e
exprIsDupable expr	     
  = go expr 0
  where
    go (Var v)   n_args = True
    go (App f a) n_args =  n_args < dupAppSize
			&& exprIsDupable a
			&& go f (n_args+1)
    go other n_args 	= False

dupAppSize :: Int
dupAppSize = 4		-- Size of application we are prepared to duplicate
\end{code}

@exprIsCheap@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form, or is cheap to get to WHNF.
[Note that that's not the same as exprIsDupable; an expression might be
big, and hence not dupable, but still cheap.]

By ``cheap'' we mean a computation we're willing to:
	push inside a lambda, or
	inline at more than one place
That might mean it gets evaluated more than once, instead of being
shared.  The main examples of things which aren't WHNF but are
``cheap'' are:

  * 	case e of
	  pi -> ei
	(where e, and all the ei are cheap)

  *	let x = e in b
	(where e and b are cheap)

  *	op x1 ... xn
	(where op is a cheap primitive operator)

  *	error "foo"
	(because we are happy to substitute it inside a lambda)

Notice that a variable is considered 'cheap': we can push it inside a lambda,
because sharing will make sure it is only evaluated once.

\begin{code}
exprIsCheap :: CoreExpr -> Bool
exprIsCheap (Lit lit) 		  = True
exprIsCheap (Type _)        	  = True
exprIsCheap (Var _)         	  = True
exprIsCheap (Note InlineMe e)  	  = True
exprIsCheap (Note _ e)      	  = exprIsCheap e
exprIsCheap (Lam x e)       	  = isRuntimeVar x || exprIsCheap e
exprIsCheap (Case e _ alts)       = exprIsCheap e && 
				    and [exprIsCheap rhs | (_,_,rhs) <- alts]
	-- Experimentally, treat (case x of ...) as cheap
	-- (and case __coerce x etc.)
	-- This improves arities of overloaded functions where
	-- there is only dictionary selection (no construction) involved
exprIsCheap (Let (NonRec x _) e)  
      | isUnLiftedType (idType x) = exprIsCheap e
      | otherwise		  = False
	-- strict lets always have cheap right hand sides, and
	-- do no allocation.

exprIsCheap other_expr 
  = go other_expr 0 True
  where
    go (Var f) n_args args_cheap 
	= (idAppIsCheap f n_args && args_cheap)
			-- A constructor, cheap primop, or partial application

	  || idAppIsBottom f n_args 
			-- Application of a function which
			-- always gives bottom; we treat this as cheap
			-- because it certainly doesn't need to be shared!
	
    go (App f a) n_args args_cheap 
	| not (isRuntimeArg a) = go f n_args 	  args_cheap
	| otherwise            = go f (n_args + 1) (exprIsCheap a && args_cheap)

    go other   n_args args_cheap = False

idAppIsCheap :: Id -> Int -> Bool
idAppIsCheap id n_val_args 
  | n_val_args == 0 = True	-- Just a type application of
				-- a variable (f t1 t2 t3)
				-- counts as WHNF
  | otherwise = case globalIdDetails id of
		  DataConId _   -> True			
		  RecordSelId _ -> True			-- I'm experimenting with making record selection
							-- look cheap, so we will substitute it inside a
							-- lambda.  Particularly for dictionary field selection

		  PrimOpId op   -> primOpIsCheap op	-- In principle we should worry about primops
		 					-- that return a type variable, since the result
							-- might be applied to something, but I'm not going
							-- to bother to check the number of args
		  other	      -> n_val_args < idArity id
\end{code}

exprOkForSpeculation returns True of an expression that it is

	* safe to evaluate even if normal order eval might not 
	  evaluate the expression at all, or

	* safe *not* to evaluate even if normal order would do so

It returns True iff

	the expression guarantees to terminate, 
	soon, 
	without raising an exception,
	without causing a side effect (e.g. writing a mutable variable)

E.G.
	let x = case y# +# 1# of { r# -> I# r# }
	in E
==>
	case y# +# 1# of { r# -> 
	let x = I# r#
	in E 
	}

We can only do this if the (y+1) is ok for speculation: it has no
side effects, and can't diverge or raise an exception.

\begin{code}
exprOkForSpeculation :: CoreExpr -> Bool
exprOkForSpeculation (Lit _)    = True
exprOkForSpeculation (Var v)    = isUnLiftedType (idType v)
exprOkForSpeculation (Note _ e) = exprOkForSpeculation e
exprOkForSpeculation other_expr
  = go other_expr 0 True
  where
    go (Var f) n_args args_ok 
      = case globalIdDetails f of
	  DataConId _ -> True	-- The strictness of the constructor has already
				-- been expressed by its "wrapper", so we don't need
				-- to take the arguments into account

	  PrimOpId op -> primOpOkForSpeculation op && args_ok
				-- A bit conservative: we don't really need
				-- to care about lazy arguments, but this is easy

	  other -> False
	
    go (App f a) n_args args_ok 
	| not (isRuntimeArg a) = go f n_args 	  args_ok
	| otherwise            = go f (n_args + 1) (exprOkForSpeculation a && args_ok)

    go other n_args args_ok = False
\end{code}


\begin{code}
exprIsBottom :: CoreExpr -> Bool	-- True => definitely bottom
exprIsBottom e = go 0 e
	       where
		-- n is the number of args
		 go n (Note _ e)   = go n e
		 go n (Let _ e)    = go n e
		 go n (Case e _ _) = go 0 e	-- Just check the scrut
		 go n (App e _)    = go (n+1) e
		 go n (Var v)      = idAppIsBottom v n
		 go n (Lit _)      = False
		 go n (Lam _ _)	   = False

idAppIsBottom :: Id -> Int -> Bool
idAppIsBottom id n_val_args = appIsBottom (idStrictness id) n_val_args
\end{code}

@exprIsValue@ returns true for expressions that are certainly *already* 
evaluated to WHNF.  This is used to decide whether it's ok to change
	case x of _ -> e   ===>   e

and to decide whether it's safe to discard a `seq`

So, it does *not* treat variables as evaluated, unless they say they are.

But it *does* treat partial applications and constructor applications
as values, even if their arguments are non-trivial; 
	e.g.  (:) (f x) (map f xs)	is a value
	      map (...redex...)		is a value
Because `seq` on such things completes immediately

A possible worry: constructors with unboxed args:
		C (f x :: Int#)
Suppose (f x) diverges; then C (f x) is not a value.  True, but
this form is illegal (see the invariants in CoreSyn).  Args of unboxed
type must be ok-for-speculation (or trivial).

\begin{code}
exprIsValue :: CoreExpr -> Bool		-- True => Value-lambda, constructor, PAP
exprIsValue (Type ty)	  = True	-- Types are honorary Values; we don't mind
					-- copying them
exprIsValue (Lit l)	  = True
exprIsValue (Lam b e)  	  = isRuntimeVar b || exprIsValue e
exprIsValue (Note _ e) 	  = exprIsValue e
exprIsValue other_expr
  = go other_expr 0
  where
    go (Var f) n_args = idAppIsValue f n_args
	
    go (App f a) n_args
	| not (isRuntimeArg a) = go f n_args
	| otherwise   	       = go f (n_args + 1) 

    go (Note _ f) n_args = go f n_args

    go other n_args = False

idAppIsValue :: Id -> Int -> Bool
idAppIsValue id n_val_args 
  = case globalIdDetails id of
	DataConId _ -> True
	PrimOpId _  -> n_val_args < idArity id
	other | n_val_args == 0 -> isEvaldUnfolding (idUnfolding id)
	      | otherwise       -> n_val_args < idArity id
	-- A worry: what if an Id's unfolding is just itself: 
	-- then we could get an infinite loop...
\end{code}

\begin{code}
exprIsConApp_maybe :: CoreExpr -> Maybe (DataCon, [CoreExpr])
exprIsConApp_maybe (Note InlineMe expr) = exprIsConApp_maybe expr
    -- We ignore InlineMe notes in case we have
    --	x = __inline_me__ (a,b)
    -- All part of making sure that INLINE pragmas never hurt
    -- Marcin tripped on this one when making dictionaries more inlinable

exprIsConApp_maybe expr = analyse (collectArgs expr)
  where
    analyse (Var fun, args)
	| Just con <- isDataConId_maybe fun,
	  length args >= dataConRepArity con
		-- Might be > because the arity excludes type args
	= Just (con,args)

	-- Look through unfoldings, but only cheap ones, because
	-- we are effectively duplicating the unfolding
    analyse (Var fun, [])
	| let unf = idUnfolding fun,
	  isCheapUnfolding unf
	= exprIsConApp_maybe (unfoldingTemplate unf)

    analyse other = Nothing
\end{code}



%************************************************************************
%*									*
\subsection{Eta reduction and expansion}
%*									*
%************************************************************************

@etaReduce@ trys an eta reduction at the top level of a Core Expr.

e.g.	\ x y -> f x y	===>  f

But we only do this if it gets rid of a whole lambda, not part.
The idea is that lambdas are often quite helpful: they indicate
head normal forms, so we don't want to chuck them away lightly.

\begin{code}
etaReduce :: CoreExpr -> CoreExpr
		-- ToDo: we should really check that we don't turn a non-bottom
		-- lambda into a bottom variable.  Sigh

etaReduce expr@(Lam bndr body)
  = check (reverse binders) body
  where
    (binders, body) = collectBinders expr

    check [] body
	| not (any (`elemVarSet` body_fvs) binders)
	= body			-- Success!
	where
	  body_fvs = exprFreeVars body

    check (b : bs) (App fun arg)
	|  (varToCoreExpr b `cheapEqExpr` arg)
	= check bs fun

    check _ _ = expr	-- Bale out

etaReduce expr = expr		-- The common case
\end{code}
	

\begin{code}
exprEtaExpandArity :: CoreExpr -> (Int, Bool) 	
-- The Int is number of value args the thing can be 
-- 	applied to without doing much work
-- The Bool is True iff there are enough explicit value lambdas
--	at the top to make this arity apparent
-- 	(but ignore it when arity==0)

-- This is used when eta expanding
--	e  ==>  \xy -> e x y
--
-- It returns 1 (or more) to:
--	case x of p -> \s -> ...
-- because for I/O ish things we really want to get that \s to the top.
-- We are prepared to evaluate x each time round the loop in order to get that
--
-- Consider	let x = expensive in \y z -> E
-- We want this to have arity 2 if the \y-abstraction is a 1-shot lambda
-- 
-- Hence the list of Bools returned by go1
--	NB: this is particularly important/useful for IO state 
--	transformers, where we often get
--		let x = E in \ s -> ...
--	and the \s is a real-world state token abstraction.  Such 
-- 	abstractions are almost invariably 1-shot, so we want to
--	pull the \s out, past the let x=E.  
-- 	The hack is in Id.isOneShotLambda

exprEtaExpandArity e
  = go 0 e
  where
    go :: Int -> CoreExpr -> (Int,Bool)
    go ar (Lam x e)  | isId x   	= go (ar+1) e
		     | otherwise 	= go ar e
    go ar (Note n e) | ok_note n	= go ar e
    go ar other 	     		= (ar + ar', ar' == 0)
					where
					  ar' = length (go1 other)

    go1 :: CoreExpr -> [Bool]
	-- (go1 e) = [b1,..,bn]
	-- means expression can be rewritten \x_b1 -> ... \x_bn -> body
	-- where bi is True <=> the lambda is one-shot

    go1 (Note n e) | ok_note n	= go1 e
    go1 (Var v)			= replicate (idArity v) False	-- When the type of the Id
								-- encodes one-shot-ness, use
								-- the idinfo here

	-- Lambdas; increase arity
    go1 (Lam x e)  | isId x     = isOneShotLambda x : go1 e
		   | otherwise	= go1 e

	-- Applications; decrease arity
    go1 (App f (Type _))	= go1 f
    go1 (App f a)  		= case go1 f of
				    (one_shot : xs) | one_shot || exprIsCheap a -> xs
				    other					-> []
							   
	-- Case/Let; keep arity if either the expression is cheap
	-- or it's a 1-shot lambda
    go1 (Case scrut _ alts) = case foldr1 (zipWith (&&)) [go1 rhs | (_,_,rhs) <- alts] of
				xs@(one_shot : _) | one_shot || exprIsCheap scrut -> xs
				other						  -> []
    go1 (Let b e) = case go1 e of
		      xs@(one_shot : _) | one_shot || all exprIsCheap (rhssOfBind b) -> xs
		      other							     -> []

    go1 other = []
    
    ok_note (Coerce _ _) = True
    ok_note InlineCall   = True
    ok_note other        = False
	    -- Notice that we do not look through __inline_me__
	    -- This may seem surprising, but consider
	    --	f = _inline_me (\x -> e)
	    -- We DO NOT want to eta expand this to
	    --	f = \x -> (_inline_me (\x -> e)) x
	    -- because the _inline_me gets dropped now it is applied, 
	    -- giving just
	    --	f = \x -> e
	    -- A Bad Idea
\end{code}


\begin{code}
etaExpand :: Int	  	-- Add this number of value args
	  -> UniqSupply
	  -> CoreExpr -> Type	-- Expression and its type
	  -> CoreExpr
-- (etaExpand n us e ty) returns an expression with 
-- the same meaning as 'e', but with arity 'n'.  

-- Given e' = etaExpand n us e ty
-- We should have
--	ty = exprType e = exprType e'
--
-- etaExpand deals with for-alls and coerces. For example:
--		etaExpand 1 E
-- where  E :: forall a. T
--	  newtype T = MkT (A -> B)
--
-- would return
--	(/\b. coerce T (\y::A -> (coerce (A->B) (E b) y)

etaExpand n us expr ty
  | n == 0 && 
    -- The ILX code generator requires eta expansion for type arguments
    -- too, but alas the 'n' doesn't tell us how many of them there 
    -- may be.  So we eagerly eta expand any big lambdas, and just
    -- cross our fingers about possible loss of sharing in the
    -- ILX case. 
    -- The Right Thing is probably to make 'arity' include
    -- type variables throughout the compiler.  (ToDo.)
    not (isForAllTy ty)	
    -- Saturated, so nothing to do
  = expr

  | otherwise	-- An unsaturated constructor or primop; eta expand it
  = case splitForAllTy_maybe ty of { 
 	  Just (tv,ty') -> Lam tv (etaExpand n us (App expr (Type (mkTyVarTy tv))) ty')

 	; Nothing ->
  
    	case splitFunTy_maybe ty of {
 	  Just (arg_ty, res_ty) -> Lam arg1 (etaExpand (n-1) us2 (App expr (Var arg1)) res_ty)
				where
				   arg1	      = mkSysLocal SLIT("eta") uniq arg_ty
 				   (us1, us2) = splitUniqSupply us
				   uniq	      = uniqFromSupply us1 
				   
	; Nothing -> pprTrace "Bad eta expand" (ppr expr $$ ppr ty) expr
    	}}
\end{code}


exprArity is a cheap-and-cheerful version of exprEtaExpandArity.
It tells how many things the expression can be applied to before doing
any work.  It doesn't look inside cases, lets, etc.  The idea is that
exprEtaExpandArity will do the hard work, leaving something that's easy
for exprArity to grapple with.  In particular, Simplify uses exprArity to
compute the ArityInfo for the Id. 

Originally I thought that it was enough just to look for top-level lambdas, but
it isn't.  I've seen this

	foo = PrelBase.timesInt

We want foo to get arity 2 even though the eta-expander will leave it
unchanged, in the expectation that it'll be inlined.  But occasionally it
isn't, because foo is blacklisted (used in a rule).  

Similarly, see the ok_note check in exprEtaExpandArity.  So 
	f = __inline_me (\x -> e)
won't be eta-expanded.

And in any case it seems more robust to have exprArity be a bit more intelligent.

\begin{code}
exprArity :: CoreExpr -> Int
exprArity e = go e
	    where
	      go (Lam x e) | isId x    	   = go e + 1
			   | otherwise 	   = go e
	      go (Note _ e)	       	   = go e
	      go (App e (Type t))      	   = go e
	      go (App f a) | exprIsCheap a = (go f - 1) `max` 0
		-- Important!  f (fac x) does not have arity 2, 
		-- 	       even if f does!
		-- NB: `max 0`!  (\x y -> f x) has arity 2, even if f is
		--		 unknown, hence arity 0
	      go (Var v) 	       	   = idArity v
	      go _		       	   = 0
\end{code}


%************************************************************************
%*									*
\subsection{Equality}
%*									*
%************************************************************************

@cheapEqExpr@ is a cheap equality test which bales out fast!
	True  => definitely equal
	False => may or may not be equal

\begin{code}
cheapEqExpr :: Expr b -> Expr b -> Bool

cheapEqExpr (Var v1)   (Var v2)   = v1==v2
cheapEqExpr (Lit lit1) (Lit lit2) = lit1 == lit2
cheapEqExpr (Type t1)  (Type t2)  = t1 `eqType` t2

cheapEqExpr (App f1 a1) (App f2 a2)
  = f1 `cheapEqExpr` f2 && a1 `cheapEqExpr` a2

cheapEqExpr _ _ = False

exprIsBig :: Expr b -> Bool
-- Returns True of expressions that are too big to be compared by cheapEqExpr
exprIsBig (Lit _)      = False
exprIsBig (Var v)      = False
exprIsBig (Type t)     = False
exprIsBig (App f a)    = exprIsBig f || exprIsBig a
exprIsBig other	       = True
\end{code}


\begin{code}
eqExpr :: CoreExpr -> CoreExpr -> Bool
	-- Works ok at more general type, but only needed at CoreExpr
	-- Used in rule matching, so when we find a type we use
	-- eqTcType, which doesn't look through newtypes
	-- [And it doesn't risk falling into a black hole either.]
eqExpr e1 e2
  = eq emptyVarEnv e1 e2
  where
  -- The "env" maps variables in e1 to variables in ty2
  -- So when comparing lambdas etc, 
  -- we in effect substitute v2 for v1 in e1 before continuing
    eq env (Var v1) (Var v2) = case lookupVarEnv env v1 of
				  Just v1' -> v1' == v2
				  Nothing  -> v1  == v2

    eq env (Lit lit1)   (Lit lit2)   = lit1 == lit2
    eq env (App f1 a1)  (App f2 a2)  = eq env f1 f2 && eq env a1 a2
    eq env (Lam v1 e1)  (Lam v2 e2)  = eq (extendVarEnv env v1 v2) e1 e2
    eq env (Let (NonRec v1 r1) e1)
	   (Let (NonRec v2 r2) e2)   = eq env r1 r2 && eq (extendVarEnv env v1 v2) e1 e2
    eq env (Let (Rec ps1) e1)
	   (Let (Rec ps2) e2)        = length ps1 == length ps2 &&
				       and (zipWith eq_rhs ps1 ps2) &&
				       eq env' e1 e2
				     where
				       env' = extendVarEnvList env [(v1,v2) | ((v1,_),(v2,_)) <- zip ps1 ps2]
				       eq_rhs (_,r1) (_,r2) = eq env' r1 r2
    eq env (Case e1 v1 a1)
	   (Case e2 v2 a2)	     = eq env e1 e2 &&
				       length a1 == length a2 &&
				       and (zipWith (eq_alt env') a1 a2)
				     where
				       env' = extendVarEnv env v1 v2

    eq env (Note n1 e1) (Note n2 e2) = eq_note env n1 n2 && eq env e1 e2
    eq env (Type t1)    (Type t2)    = t1 `eqType` t2
    eq env e1		e2	     = False
				         
    eq_list env []	 []	  = True
    eq_list env (e1:es1) (e2:es2) = eq env e1 e2 && eq_list env es1 es2
    eq_list env es1      es2      = False
    
    eq_alt env (c1,vs1,r1) (c2,vs2,r2) = c1==c2 &&
					 eq (extendVarEnvList env (vs1 `zip` vs2)) r1 r2

    eq_note env (SCC cc1)      (SCC cc2)      = cc1 == cc2
    eq_note env (Coerce t1 f1) (Coerce t2 f2) = t1 `eqType` t2 && f1 `eqType` f2
    eq_note env InlineCall     InlineCall     = True
    eq_note env other1	       other2	      = False
\end{code}


%************************************************************************
%*									*
\subsection{The size of an expression}
%*									*
%************************************************************************

\begin{code}
coreBindsSize :: [CoreBind] -> Int
coreBindsSize bs = foldr ((+) . bindSize) 0 bs

exprSize :: CoreExpr -> Int
	-- A measure of the size of the expressions
	-- It also forces the expression pretty drastically as a side effect
exprSize (Var v)       = varSize v 
exprSize (Lit lit)     = lit `seq` 1
exprSize (App f a)     = exprSize f + exprSize a
exprSize (Lam b e)     = varSize b + exprSize e
exprSize (Let b e)     = bindSize b + exprSize e
exprSize (Case e b as) = exprSize e + varSize b + foldr ((+) . altSize) 0 as
exprSize (Note n e)    = noteSize n + exprSize e
exprSize (Type t)      = seqType t `seq` 1

noteSize (SCC cc)       = cc `seq` 1
noteSize (Coerce t1 t2) = seqType t1 `seq` seqType t2 `seq` 1
noteSize InlineCall     = 1
noteSize InlineMe       = 1

varSize :: Var -> Int
varSize b  | isTyVar b = 1
	   | otherwise = seqType (idType b)		`seq`
			 megaSeqIdInfo (idInfo b) 	`seq`
			 1

varsSize = foldr ((+) . varSize) 0

bindSize (NonRec b e) = varSize b + exprSize e
bindSize (Rec prs)    = foldr ((+) . pairSize) 0 prs

pairSize (b,e) = varSize b + exprSize e

altSize (c,bs,e) = c `seq` varsSize bs + exprSize e
\end{code}


%************************************************************************
%*									*
\subsection{Hashing}
%*									*
%************************************************************************

\begin{code}
hashExpr :: CoreExpr -> Int
hashExpr e | hash < 0  = 77	-- Just in case we hit -maxInt
	   | otherwise = hash
	   where
	     hash = abs (hash_expr e)	-- Negative numbers kill UniqFM

hash_expr (Note _ e)   		  = hash_expr e
hash_expr (Let (NonRec b r) e)    = hashId b
hash_expr (Let (Rec ((b,r):_)) e) = hashId b
hash_expr (Case _ b _)		  = hashId b
hash_expr (App f e)   		  = hash_expr f * fast_hash_expr e
hash_expr (Var v)     		  = hashId v
hash_expr (Lit lit)	   	  = hashLiteral lit
hash_expr (Lam b _)	          = hashId b
hash_expr (Type t)	          = trace "hash_expr: type" 1		-- Shouldn't happen

fast_hash_expr (Var v)     	= hashId v
fast_hash_expr (Lit lit)	= hashLiteral lit
fast_hash_expr (App f (Type _)) = fast_hash_expr f
fast_hash_expr (App f a)        = fast_hash_expr a
fast_hash_expr (Lam b _)        = hashId b
fast_hash_expr other	        = 1

hashId :: Id -> Int
hashId id = hashName (idName id)
\end{code}
