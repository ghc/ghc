%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module CoreUtils (
	coreExprType, coreAltsType,

	exprIsBottom, exprIsDupable, exprIsTrivial, exprIsCheap, exprIsValue,
	exprOkForSpeculation, exprIsBig, hashExpr,
	exprArity,
	cheapEqExpr, eqExpr, applyTypeToArgs
    ) where

#include "HsVersions.h"


import {-# SOURCE #-} CoreUnfold	( isEvaldUnfolding )

import CoreSyn
import PprCore		( pprCoreExpr )
import Var		( IdOrTyVar, isId, isTyVar )
import VarSet
import VarEnv
import Name		( isLocallyDefined, hashName )
import Const		( Con, isWHNFCon, conIsTrivial, conIsCheap, conIsDupable,
			  conType, conOkForSpeculation, conStrictness, hashCon
			)
import Id		( Id, idType, setIdType, idUnique, idAppIsBottom,
			  getIdArity, idName,
			  getIdSpecialisation, setIdSpecialisation,
			  getInlinePragma, setInlinePragma,
			  getIdUnfolding, setIdUnfolding, idInfo
			)
import IdInfo		( arityLowerBound, InlinePragInfo(..), lbvarInfo, LBVarInfo(..) )
import Type		( Type, mkFunTy, mkForAllTy,
			  splitFunTy_maybe, tyVarsOfType, tyVarsOfTypes,
                          isNotUsgTy, mkUsgTy, unUsgTy, UsageAnn(..),
			  tidyTyVar, applyTys, isUnLiftedType
			)
import Demand		( isPrim, isLazy )
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
coreExprType :: CoreExpr -> Type

coreExprType (Var var)		    = idType var
coreExprType (Let _ body)	    = coreExprType body
coreExprType (Case _ _ alts)        = coreAltsType alts
coreExprType (Note (Coerce ty _) e) = ty
coreExprType (Note (TermUsg u) e)   = mkUsgTy u (unUsgTy (coreExprType e))
coreExprType (Note other_note e)    = coreExprType e
coreExprType e@(Con con args)       = applyTypeToArgs e (conType con) args

coreExprType (Lam binder expr)
  | isId binder    = (case (lbvarInfo . idInfo) binder of
                       IsOneShotLambda -> mkUsgTy UsOnce
                       otherwise       -> id) $
                     idType binder `mkFunTy` coreExprType expr
  | isTyVar binder = mkForAllTy binder (coreExprType expr)

coreExprType e@(App _ _)
  = case collectArgs e of
	(fun, args) -> applyTypeToArgs e (coreExprType fun) args

coreExprType other = pprTrace "coreExprType" (pprCoreExpr other) alphaTy

coreAltsType :: [CoreAlt] -> Type
coreAltsType ((_,_,rhs) : _) = coreExprType rhs
\end{code}

\begin{code}
-- The first argument is just for debugging
applyTypeToArgs :: CoreExpr -> Type -> [CoreExpr] -> Type
applyTypeToArgs e op_ty [] = op_ty

applyTypeToArgs e op_ty (Type ty : args)
  =	-- Accumulate type arguments so we can instantiate all at once
    ASSERT2( all isNotUsgTy tys, ppr e <+> text "of" <+> ppr op_ty <+> text "to" <+> ppr (Type ty : args) <+> text "i.e." <+> ppr tys )
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
\subsection{Figuring out things about expressions}
%*									*
%************************************************************************

@exprIsTrivial@	is true of expressions we are unconditionally 
		happy to duplicate; simple variables and constants,
		and type applications.

@exprIsBottom@	is true of expressions that are guaranteed to diverge


\begin{code}
exprIsTrivial (Type _)	     = True
exprIsTrivial (Var v) 	     = True
exprIsTrivial (App e arg)    = isTypeArg arg && exprIsTrivial e
exprIsTrivial (Note _ e)     = exprIsTrivial e
exprIsTrivial (Con con args) = conIsTrivial con && all isTypeArg args
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
exprIsDupable (Con con args) = conIsDupable con && 
			       all exprIsDupable args &&
			       valArgCount args <= dupAppSize

exprIsDupable (Note _ e)     = exprIsDupable e
exprIsDupable expr	     = case collectArgs expr of  
				  (Var f, args) ->  valArgCount args <= dupAppSize
				  other		->  False

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
exprIsCheap (Type _)        	= True
exprIsCheap (Var _)         	= True
exprIsCheap (Con con args)  	= conIsCheap con && all exprIsCheap args
exprIsCheap (Note _ e)      	= exprIsCheap e
exprIsCheap (Lam x e)       	= if isId x then True else exprIsCheap e

--	I'm not at all convinced about these two!!
--	[SLPJ June 99]
-- exprIsCheap (Let bind body) 	= all exprIsCheap (rhssOfBind bind) && exprIsCheap body
-- exprIsCheap (Case scrut _ alts) = exprIsCheap scrut && 
--		  		     all (\(_,_,rhs) -> exprIsCheap rhs) alts

exprIsCheap other_expr   -- look for manifest partial application
  = case collectArgs other_expr of
	(f, args) -> isPap f (valArgCount args) && all exprIsCheap args
\end{code}

\begin{code}
isPap :: CoreExpr		-- Function
      -> Int			-- Number of value args
      -> Bool
isPap (Var f) n_val_args 
  =    idAppIsBottom f n_val_args 
				-- Application of a function which
				-- always gives bottom; we treat this as
				-- a WHNF, because it certainly doesn't
				-- need to be shared!

    || n_val_args == 0 		-- Just a type application of
				-- a variable (f t1 t2 t3)
				-- counts as WHNF

    || n_val_args < arityLowerBound (getIdArity f)
		
isPap fun n_val_args = False
\end{code}

exprOkForSpeculation returns True of an UNLIFTED-TYPE expression that it is safe
to evaluate even if normal order eval might not evaluate the expression 
at all.  E.G.
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
exprOkForSpeculation (Var v)        = True	-- Unlifted type => already evaluated

exprOkForSpeculation (Note _ e)     	  = exprOkForSpeculation e
exprOkForSpeculation (Let (NonRec b r) e) = isUnLiftedType (idType b) && 
					    exprOkForSpeculation r && 
					    exprOkForSpeculation e
exprOkForSpeculation (Let (Rec _) _) = False
exprOkForSpeculation (Case _ _ _)    = False	-- Conservative
exprOkForSpeculation (App _ _)       = False

exprOkForSpeculation (Con con args)
  = conOkForSpeculation con &&
    and (zipWith ok (filter isValArg args) (fst (conStrictness con)))
  where
    ok arg demand | isLazy demand = True
		  | isPrim demand = exprOkForSpeculation arg
		  | otherwise	  = False

exprOkForSpeculation other = panic "exprOkForSpeculation"
	-- Lam, Type
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
		 go n (Con _ _)    = False
		 go n (Lam _ _)	   = False
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
exprIsValue (Var v)    	  = isEvaldUnfolding (getIdUnfolding v)
exprIsValue (Lam b e)  	  = isId b || exprIsValue e
exprIsValue (Note _ e) 	  = exprIsValue e
exprIsValue (Let _ e)     = False
exprIsValue (Case _ _ _)  = False
exprIsValue (Con con _)   = isWHNFCon con 
exprIsValue e@(App _ _)   = case collectArgs e of  
				  (Var v, args) -> fun_arity > valArgCount args
						where
						   fun_arity  = arityLowerBound (getIdArity v)
				  _	        -> False
\end{code}

\begin{code}
exprArity :: CoreExpr -> Int	-- How many value lambdas are at the top
exprArity (Lam b e) | isTyVar b = exprArity e
		    | otherwise = 1 + exprArity e
exprArity other			= 0
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

cheapEqExpr (Var v1) (Var v2) = v1==v2
cheapEqExpr (Con con1 args1) (Con con2 args2)
  = con1 == con2 && 
    and (zipWithEqual "cheapEqExpr" cheapEqExpr args1 args2)

cheapEqExpr (App f1 a1) (App f2 a2)
  = f1 `cheapEqExpr` f2 && a1 `cheapEqExpr` a2

cheapEqExpr (Type t1) (Type t2) = t1 == t2

cheapEqExpr _ _ = False

exprIsBig :: Expr b -> Bool
-- Returns True of expressions that are too big to be compared by cheapEqExpr
exprIsBig (Var v)      = False
exprIsBig (Type t)     = False
exprIsBig (App f a)    = exprIsBig f || exprIsBig a
exprIsBig (Con _ args) = any exprIsBig args
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

    eq env (Con c1 es1) (Con c2 es2) = c1 == c2 && eq_list env es1 es2
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
    eq_note env (Coerce f1 t1) (Coerce f2 t2) = f1==f2 && t1==t2
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
hashExpr (Note _ e)   		 = hashExpr e
hashExpr (Let (NonRec b r) e)    = hashId b
hashExpr (Let (Rec ((b,r):_)) e) = hashId b
hashExpr (Case _ b _)		 = hashId b
hashExpr (App f e)   		 = hashExpr f
hashExpr (Var v)     		 = hashId v
hashExpr (Con con args)   	 = hashArgs args (hashCon con)
hashExpr (Lam b _)	         = hashId b
hashExpr (Type t)	         = trace "hashExpr: type" 0		-- Shouldn't happen

hashArgs []		 con = con
hashArgs (Type t : args) con = hashArgs args con
hashArgs (arg    : args) con = hashExpr arg

hashId :: Id -> Int
hashId id = hashName (idName id)
\end{code}
