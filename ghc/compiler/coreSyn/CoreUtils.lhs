%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module CoreUtils (
	exprType, coreAltsType,

	mkNote, mkInlineMe, mkSCC, mkCoerce,

	exprIsBottom, exprIsDupable, exprIsTrivial, exprIsCheap, 
	exprIsValue,exprOkForSpeculation, exprIsBig, 
	exprArity, 

	idAppIsBottom, idAppIsCheap,

	etaReduceExpr, exprEtaExpandArity,

	hashExpr,

	cheapEqExpr, eqExpr, applyTypeToArgs
    ) where

#include "HsVersions.h"


import {-# SOURCE #-} CoreUnfold	( isEvaldUnfolding )

import GlaExts		-- For `xori` 

import CoreSyn
import CoreFVs		( exprFreeVars )
import PprCore		( pprCoreExpr )
import Var		( isId, isTyVar )
import VarSet
import VarEnv
import Name		( isLocallyDefined, hashName )
import Literal		( Literal, hashLiteral, literalType )
import PrimOp		( primOpOkForSpeculation, primOpIsCheap )
import Id		( Id, idType, idFlavour, idStrictness, idLBVarInfo, 
			  idArity, idName, idUnfolding, idInfo
			)
import IdInfo		( arityLowerBound, InlinePragInfo(..),
			  LBVarInfo(..),  
			  IdFlavour(..),
			  appIsBottom
			)
import Type		( Type, mkFunTy, mkForAllTy,
			  splitFunTy_maybe, tyVarsOfType, tyVarsOfTypes,
                          isNotUsgTy, mkUsgTy, unUsgTy, UsageAnn(..),
			  applyTys, isUnLiftedType
			)
import CostCentre	( CostCentre )
import Unique		( buildIdKey, augmentIdKey )
import Util		( zipWithEqual, mapAccumL )
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
exprType (Note (TermUsg u) e)   = mkUsgTy u (unUsgTy (exprType e))
exprType (Note other_note e)    = exprType e
exprType (Lam binder expr)
  | isId binder    = (case idLBVarInfo binder of
                       IsOneShotLambda -> mkUsgTy UsOnce
                       otherwise       -> id) $
                     idType binder `mkFunTy` exprType expr
  | isTyVar binder = mkForAllTy binder (exprType expr)

exprType e@(App _ _)
  = case collectArgs e of
	(fun, args) -> applyTypeToArgs e (exprType fun) args

exprType other = pprTrace "exprType" (pprCoreExpr other) alphaTy

coreAltsType :: [CoreAlt] -> Type
coreAltsType ((_,_,rhs) : _) = exprType rhs
\end{code}

\begin{code}
-- The first argument is just for debugging
applyTypeToArgs :: CoreExpr -> Type -> [CoreExpr] -> Type
applyTypeToArgs e op_ty [] = op_ty

applyTypeToArgs e op_ty (Type ty : args)
  =	-- Accumulate type arguments so we can instantiate all at once
    ASSERT2( all isNotUsgTy tys, 
	     ppr e <+> text "of" <+> ppr op_ty <+> text "to" <+> 
	     ppr (Type ty : args) <+> text "i.e." <+> ppr tys )
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
\subsection{Attaching notes
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

\begin{code}
mkInlineMe e | exprIsTrivial e = e
	     | otherwise       = Note InlineMe e
\end{code}



\begin{code}
mkCoerce :: Type -> Type -> Expr b -> Expr b
-- In (mkCoerce to_ty from_ty e), we require that from_ty = exprType e
-- But exprType is defined in CoreUtils, so we don't check the assertion

mkCoerce to_ty from_ty (Note (Coerce to_ty2 from_ty2) expr)
  = ASSERT( from_ty == to_ty2 )
    mkCoerce to_ty from_ty2 expr

mkCoerce to_ty from_ty expr
  | to_ty == from_ty = expr
  | otherwise	     = Note (Coerce to_ty from_ty) expr
\end{code}

\begin{code}
mkSCC :: CostCentre -> Expr b -> Expr b
	-- Note: Nested SCC's *are* preserved for the benefit of
	--       cost centre stack profiling (Durham)

mkSCC cc (Lit lit) = Lit lit
mkSCC cc (Lam x e) = Lam x (mkSCC cc e)	-- Move _scc_ inside lambda
mkSCC cc expr	   = Note (SCC cc) expr
\end{code}


%************************************************************************
%*									*
\subsection{Figuring out things about expressions}
%*									*
%************************************************************************

@exprIsTrivial@	is true of expressions we are unconditionally 
		happy to duplicate; simple variables and constants,
		and type applications.

@exprIsBottom@	is true of expressions that are guaranteed to diverge


\begin{code}
exprIsTrivial (Type _)	     = True
exprIsTrivial (Lit lit)      = True
exprIsTrivial (Var v) 	     = True
exprIsTrivial (App e arg)    = isTypeArg arg && exprIsTrivial e
exprIsTrivial (Note _ e)     = exprIsTrivial e
exprIsTrivial (Lam b body)   | isTyVar b = exprIsTrivial body
exprIsTrivial other	     = False
\end{code}


@exprIsDupable@	is true of expressions that can be duplicated at a modest
		cost in code size.  This will only happen in different case
		branches, so there's no issue about duplicating work.

		That is, exprIsDupable returns True of (f x) even if
		f is very very expensive to call.

		Its only purpose is to avoid fruitless let-binding
		and then inlining of case join points


\begin{code}
exprIsDupable (Type _)	     = True
exprIsDupable (Var v)	     = True
exprIsDupable (Lit lit)      = True
exprIsDupable (Note _ e)     = exprIsDupable e
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

	where e, and all the ei are cheap; and

  *	let x = e
	in b

	where e and b are cheap; and

  *	op x1 ... xn

	where op is a cheap primitive operator

  *	error "foo"

Notice that a variable is considered 'cheap': we can push it inside a lambda,
because sharing will make sure it is only evaluated once.

\begin{code}
exprIsCheap :: CoreExpr -> Bool
exprIsCheap (Lit lit) 		  = True
exprIsCheap (Type _)        	  = True
exprIsCheap (Var _)         	  = True
exprIsCheap (Note _ e)      	  = exprIsCheap e
exprIsCheap (Lam x e)       	  = if isId x then True else exprIsCheap e
exprIsCheap (Case (Var v) _ alts) = and [exprIsCheap rhs | (_,_,rhs) <- alts]
	-- Experimentally, treat (case x of ...) as cheap
	-- This improves arities of overloaded functions where
	-- there is only dictionary selection (no construction) involved
exprIsCheap other_expr 
  = go other_expr 0 True
  where
    go (Var f) n_args args_cheap 
	= (idAppIsCheap f n_args && args_cheap)
			-- A constructor, cheap primop, or partial application

	  || idAppIsBottom f n_args 
			-- Application of a function which
			-- always gives bottom; we treat this as
			-- a WHNF, because it certainly doesn't
			-- need to be shared!
	
    go (App f a) n_args args_cheap 
	| isTypeArg a = go f n_args 	  args_cheap
	| otherwise   = go f (n_args + 1) (exprIsCheap a && args_cheap)

    go other   n_args args_cheap = False

idAppIsCheap :: Id -> Int -> Bool
idAppIsCheap id n_val_args 
  | n_val_args == 0 = True	-- Just a type application of
				-- a variable (f t1 t2 t3)
				-- counts as WHNF
  | otherwise = case idFlavour id of
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
      = case idFlavour f of
	  DataConId _ -> True	-- The strictness of the constructor has already
				-- been expressed by its "wrapper", so we don't need
				-- to take the arguments into account

	  PrimOpId op -> primOpOkForSpeculation op && args_ok
				-- A bit conservative: we don't really need
				-- to care about lazy arguments, but this is easy

	  other -> False
	
    go (App f a) n_args args_ok 
	| isTypeArg a = go f n_args 	  args_ok
	| otherwise   = go f (n_args + 1) (exprOkForSpeculation a && args_ok)

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
evaluated to WHNF.  This is used to decide wether it's ok to change
	case x of _ -> e   ===>   e

and to decide whether it's safe to discard a `seq`

So, it does *not* treat variables as evaluated, unless they say they are

\begin{code}
exprIsValue :: CoreExpr -> Bool		-- True => Value-lambda, constructor, PAP
exprIsValue (Type ty)	  = True	-- Types are honorary Values; we don't mind
					-- copying them
exprIsValue (Lit l)	  = True
exprIsValue (Lam b e)  	  = isId b || exprIsValue e
exprIsValue (Note _ e) 	  = exprIsValue e
exprIsValue other_expr
  = go other_expr 0
  where
    go (Var f) n_args = idAppIsValue f n_args
	
    go (App f a) n_args
	| isTypeArg a = go f n_args
	| otherwise   = go f (n_args + 1) 

    go (Note _ f) n_args = go f n_args

    go other n_args = False

idAppIsValue :: Id -> Int -> Bool
idAppIsValue id n_val_args 
  = case idFlavour id of
	DataConId _ -> True
	PrimOpId _  -> n_val_args < idArity id
	other | n_val_args == 0 -> isEvaldUnfolding (idUnfolding id)
	      | otherwise       -> n_val_args < idArity id
	-- A worry: what if an Id's unfolding is just itself: 
	-- then we could get an infinite loop...
\end{code}

\begin{code}
exprArity :: CoreExpr -> Int	-- How many value lambdas are at the top
exprArity (Lam b e)     | isTyVar b	= exprArity e
		        | otherwise	= 1 + exprArity e

exprArity (Note note e) | ok_note note	= exprArity e
			where
			  ok_note (Coerce _ _) = True
				-- We *do* look through coerces when getting arities.
				-- Reason: arities are to do with *representation* and
				-- work duplication. 
			  ok_note InlineMe     = True
			  ok_note InlineCall   = True
			  ok_note other	       = False
				-- SCC and TermUsg might be over-conservative?

exprArity other	= 0
\end{code}


%************************************************************************
%*									*
\subsection{Eta reduction and expansion}
%*									*
%************************************************************************

@etaReduceExpr@ trys an eta reduction at the top level of a Core Expr.

e.g.	\ x y -> f x y	===>  f

But we only do this if it gets rid of a whole lambda, not part.
The idea is that lambdas are often quite helpful: they indicate
head normal forms, so we don't want to chuck them away lightly.

\begin{code}
etaReduceExpr :: CoreExpr -> CoreExpr
		-- ToDo: we should really check that we don't turn a non-bottom
		-- lambda into a bottom variable.  Sigh

etaReduceExpr expr@(Lam bndr body)
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

etaReduceExpr expr = expr		-- The common case
\end{code}
	

\begin{code}
exprEtaExpandArity :: CoreExpr -> Int 	-- The number of args the thing can be applied to
					-- without doing much work
-- This is used when eta expanding
--	e  ==>  \xy -> e x y
--
-- It returns 1 (or more) to:
--	case x of p -> \s -> ...
-- because for I/O ish things we really want to get that \s to the top.
-- We are prepared to evaluate x each time round the loop in order to get that
-- Hence "generous" arity

exprEtaExpandArity e
  = go e
  where
    go (Var v)         			= idArity v
    go (App f (Type _))			= go f
    go (App f a)  | exprIsCheap a	= (go f - 1) `max` 0	-- Never go -ve!
    go (Lam x e)  | isId x    		= go e + 1
		  | otherwise 		= go e
    go (Note n e) | ok_note n		= go e
    go (Case scrut _ alts)
      | exprIsCheap scrut		= min_zero [go rhs | (_,_,rhs) <- alts]
    go (Let b e) 	
      | all exprIsCheap (rhssOfBind b)	= go e
    
    go other 				= 0
    
    ok_note (Coerce _ _) = True
    ok_note InlineCall   = True
    ok_note other        = False
	    -- Notice that we do not look through __inline_me__
	    -- This one is a bit more surprising, but consider
	    --	f = _inline_me (\x -> e)
	    -- We DO NOT want to eta expand this to
	    --	f = \x -> (_inline_me (\x -> e)) x
	    -- because the _inline_me gets dropped now it is applied, 
	    -- giving just
	    --	f = \x -> e
	    -- A Bad Idea

min_zero :: [Int] -> Int	-- Find the minimum, but zero is the smallest
min_zero (x:xs) = go x xs
		where
		  go 0   xs		    = 0		-- Nothing beats zero
		  go min []	  	    = min
		  go min (x:xs) | x < min   = go x xs
				| otherwise = go min xs 

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
cheapEqExpr (Type t1)  (Type t2)  = t1 == t2

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
    eq env (Type t1)    (Type t2)    = t1 == t2
    eq env e1		e2	     = False
				         
    eq_list env []	 []	  = True
    eq_list env (e1:es1) (e2:es2) = eq env e1 e2 && eq_list env es1 es2
    eq_list env es1      es2      = False
    
    eq_alt env (c1,vs1,r1) (c2,vs2,r2) = c1==c2 &&
					 eq (extendVarEnvList env (vs1 `zip` vs2)) r1 r2

    eq_note env (SCC cc1)      (SCC cc2)      = cc1 == cc2
    eq_note env (Coerce t1 f1) (Coerce t2 f2) = t1==t2 && f1==f2
    eq_note env InlineCall     InlineCall     = True
    eq_note env other1	       other2	      = False
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
