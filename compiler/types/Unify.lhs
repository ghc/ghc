%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module Unify ( 
	-- Matching of types: 
	--	the "tc" prefix indicates that matching always
	--	respects newtypes (rather than looking through them)
	tcMatchTy, tcMatchTys, tcMatchTyX, 
	ruleMatchTyX, tcMatchPreds, 

	MatchEnv(..), matchList, 

	typesCantMatch,

        -- Side-effect free unification
        tcUnifyTy, tcUnifyTys, BindFlag(..),

        UnifyResultM(..), UnifyResult, tcUnifyTysFG

   ) where

#include "HsVersions.h"

import Var
import VarEnv
import VarSet
import Kind
import Type
import TyCon
import TypeRep
import Util
import PrelNames(typeNatKindConNameKey, typeSymbolKindConNameKey)
import Unique(hasKey)

import Control.Monad (liftM, ap, unless, guard)
import Control.Applicative (Applicative(..))
\end{code}


%************************************************************************
%*									*
		Matching
%*									*
%************************************************************************


Matching is much tricker than you might think.

1. The substitution we generate binds the *template type variables*
   which are given to us explicitly.

2. We want to match in the presence of foralls; 
	e.g 	(forall a. t1) ~ (forall b. t2)

   That is what the RnEnv2 is for; it does the alpha-renaming
   that makes it as if a and b were the same variable.
   Initialising the RnEnv2, so that it can generate a fresh
   binder when necessary, entails knowing the free variables of
   both types.

3. We must be careful not to bind a template type variable to a
   locally bound variable.  E.g.
	(forall a. x) ~ (forall b. b)
   where x is the template type variable.  Then we do not want to
   bind x to a/b!  This is a kind of occurs check.
   The necessary locals accumulate in the RnEnv2.


\begin{code}
data MatchEnv
  = ME	{ me_tmpls :: VarSet	-- Template variables
 	, me_env   :: RnEnv2	-- Renaming envt for nested foralls
	}			--   In-scope set includes template variables
    -- Nota Bene: MatchEnv isn't specific to Types.  It is used
    --            for matching terms and coercions as well as types

tcMatchTy :: TyVarSet		-- Template tyvars
	  -> Type		-- Template
	  -> Type		-- Target
	  -> Maybe TvSubst	-- One-shot; in principle the template
				-- variables could be free in the target

tcMatchTy tmpls ty1 ty2
  = case match menv emptyTvSubstEnv ty1 ty2 of
	Just subst_env -> Just (TvSubst in_scope subst_env)
	Nothing	       -> Nothing
  where
    menv     = ME { me_tmpls = tmpls, me_env = mkRnEnv2 in_scope }
    in_scope = mkInScopeSet (tmpls `unionVarSet` tyVarsOfType ty2)
	-- We're assuming that all the interesting 
	-- tyvars in tys1 are in tmpls

tcMatchTys :: TyVarSet		-- Template tyvars
	   -> [Type]		-- Template
	   -> [Type]		-- Target
	   -> Maybe TvSubst	-- One-shot; in principle the template
				-- variables could be free in the target

tcMatchTys tmpls tys1 tys2
  = case match_tys menv emptyTvSubstEnv tys1 tys2 of
	Just subst_env -> Just (TvSubst in_scope subst_env)
	Nothing	       -> Nothing
  where
    menv     = ME { me_tmpls = tmpls, me_env = mkRnEnv2 in_scope }
    in_scope = mkInScopeSet (tmpls `unionVarSet` tyVarsOfTypes tys2)
	-- We're assuming that all the interesting 
	-- tyvars in tys1 are in tmpls

-- This is similar, but extends a substitution
tcMatchTyX :: TyVarSet 		-- Template tyvars
	   -> TvSubst		-- Substitution to extend
	   -> Type		-- Template
	   -> Type		-- Target
	   -> Maybe TvSubst
tcMatchTyX tmpls (TvSubst in_scope subst_env) ty1 ty2
  = case match menv subst_env ty1 ty2 of
	Just subst_env -> Just (TvSubst in_scope subst_env)
	Nothing	       -> Nothing
  where
    menv = ME {me_tmpls = tmpls, me_env = mkRnEnv2 in_scope}

tcMatchPreds
	:: [TyVar]			-- Bind these
	-> [PredType] -> [PredType]
   	-> Maybe TvSubstEnv
tcMatchPreds tmpls ps1 ps2
  = matchList (match menv) emptyTvSubstEnv ps1 ps2
  where
    menv = ME { me_tmpls = mkVarSet tmpls, me_env = mkRnEnv2 in_scope_tyvars }
    in_scope_tyvars = mkInScopeSet (tyVarsOfTypes ps1 `unionVarSet` tyVarsOfTypes ps2)

-- This one is called from the expression matcher, which already has a MatchEnv in hand
ruleMatchTyX :: MatchEnv 
	 -> TvSubstEnv		-- Substitution to extend
	 -> Type		-- Template
	 -> Type		-- Target
	 -> Maybe TvSubstEnv

ruleMatchTyX menv subst ty1 ty2 = match menv subst ty1 ty2	-- Rename for export
\end{code}

Now the internals of matching

\begin{code}
match :: MatchEnv	-- For the most part this is pushed downwards
      -> TvSubstEnv 	-- Substitution so far:
			--   Domain is subset of template tyvars
			--   Free vars of range is subset of 
			--	in-scope set of the RnEnv2
      -> Type -> Type	-- Template and target respectively
      -> Maybe TvSubstEnv

match menv subst ty1 ty2 | Just ty1' <- coreView ty1 = match menv subst ty1' ty2
			 | Just ty2' <- coreView ty2 = match menv subst ty1 ty2'

match menv subst (TyVarTy tv1) ty2
  | Just ty1' <- lookupVarEnv subst tv1'	-- tv1' is already bound
  = if eqTypeX (nukeRnEnvL rn_env) ty1' ty2
	-- ty1 has no locally-bound variables, hence nukeRnEnvL
    then Just subst
    else Nothing	-- ty2 doesn't match

  | tv1' `elemVarSet` me_tmpls menv
  = if any (inRnEnvR rn_env) (varSetElems (tyVarsOfType ty2))
    then Nothing	-- Occurs check
    else do { subst1 <- match_kind menv subst (tyVarKind tv1) (typeKind ty2)
			-- Note [Matching kinds]
            ; guard (validKindShape (tyVarKind tv1) ty2)
                        -- Note [Kinds Containing Only Literals]
	    ; return (extendVarEnv subst1 tv1' ty2) }

   | otherwise	-- tv1 is not a template tyvar
   = case ty2 of
	TyVarTy tv2 | tv1' == rnOccR rn_env tv2 -> Just subst
	_                                       -> Nothing
  where
    rn_env = me_env menv
    tv1' = rnOccL rn_env tv1

match menv subst (ForAllTy tv1 ty1) (ForAllTy tv2 ty2) 
  = do { subst' <- match_kind menv subst (tyVarKind tv1) (tyVarKind tv2)
       ; match menv' subst' ty1 ty2 }
  where		-- Use the magic of rnBndr2 to go under the binders
    menv' = menv { me_env = rnBndr2 (me_env menv) tv1 tv2 }

match menv subst (TyConApp tc1 tys1) (TyConApp tc2 tys2) 
  | tc1 == tc2 = match_tys menv subst tys1 tys2
match menv subst (FunTy ty1a ty1b) (FunTy ty2a ty2b) 
  = do { subst' <- match menv subst ty1a ty2a
       ; match menv subst' ty1b ty2b }
match menv subst (AppTy ty1a ty1b) ty2
  | Just (ty2a, ty2b) <- repSplitAppTy_maybe ty2
	-- 'repSplit' used because the tcView stuff is done above
  = do { subst' <- match menv subst ty1a ty2a
       ; match menv subst' ty1b ty2b }

match _ subst (LitTy x) (LitTy y) | x == y  = return subst

match _ _ _ _
  = Nothing


{- Note [Kinds Containing Only Literals]

The kinds `Nat` and `Symbol` contain only literal types (e.g., 17, "Hi", etc.).
As such, they can only ever match and unify with a type variable or a literal
type.  We check for this during matching and unification, and reject
binding variables to types that have an unacceptable shape.

This helps us avoid "overlapping instance" errors in the presence of
very general instances.   The main motivating example for this is the
implementation of `Typeable`, which conatins the instances:

... => Typeable (f a) where ...
... => Typeable (a :: Nat) where ...

Without the explicit check these look like they overlap, and are rejected.
The two do not overlap, however, because nothing of kind `Nat` can be
of the form `f a`.
-}

validKindShape :: Kind -> Type -> Bool
validKindShape k ty
  | Just (tc,[]) <- splitTyConApp_maybe k
  , tc `hasKey` typeNatKindConNameKey ||
    tc `hasKey` typeSymbolKindConNameKey = case ty of
                                             TyVarTy _ -> True
                                             LitTy _   -> True
                                             _         -> False
validKindShape _ _ = True


--------------
match_kind :: MatchEnv -> TvSubstEnv -> Kind -> Kind -> Maybe TvSubstEnv
-- Match the kind of the template tyvar with the kind of Type
-- Note [Matching kinds]
match_kind menv subst k1 k2
  | k2 `isSubKind` k1
  = return subst

  | otherwise
  = match menv subst k1 k2

-- Note [Matching kinds]
-- ~~~~~~~~~~~~~~~~~~~~~
-- For ordinary type variables, we don't want (m a) to match (n b) 
-- if say (a::*) and (b::*->*).  This is just a yes/no issue. 
--
-- For coercion kinds matters are more complicated.  If we have a 
-- coercion template variable co::a~[b], where a,b are presumably also
-- template type variables, then we must match co's kind against the 
-- kind of the actual argument, so as to give bindings to a,b.  
--
-- In fact I have no example in mind that *requires* this kind-matching
-- to instantiate template type variables, but it seems like the right
-- thing to do.  C.f. Note [Matching variable types] in Rules.lhs

--------------
match_tys :: MatchEnv -> TvSubstEnv -> [Type] -> [Type] -> Maybe TvSubstEnv
match_tys menv subst tys1 tys2 = matchList (match menv) subst tys1 tys2

--------------
matchList :: (env -> a -> b -> Maybe env)
	   -> env -> [a] -> [b] -> Maybe env
matchList _  subst []     []     = Just subst
matchList fn subst (a:as) (b:bs) = do { subst' <- fn subst a b
				      ; matchList fn subst' as bs }
matchList _  _     _      _      = Nothing
\end{code}


%************************************************************************
%*									*
		GADTs
%*									*
%************************************************************************

Note [Pruning dead case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider	data T a where
		   T1 :: T Int
		   T2 :: T a

		newtype X = MkX Int
		newtype Y = MkY Char

		type family F a
		type instance F Bool = Int

Now consider	case x of { T1 -> e1; T2 -> e2 }

The question before the house is this: if I know something about the type
of x, can I prune away the T1 alternative?

Suppose x::T Char.  It's impossible to construct a (T Char) using T1, 
	Answer = YES we can prune the T1 branch (clearly)

Suppose x::T (F a), where 'a' is in scope.  Then 'a' might be instantiated
to 'Bool', in which case x::T Int, so
	ANSWER = NO (clearly)

Suppose x::T X.  Then *in Haskell* it's impossible to construct a (non-bottom) 
value of type (T X) using T1.  But *in FC* it's quite possible.  The newtype
gives a coercion
	CoX :: X ~ Int
So (T CoX) :: T X ~ T Int; hence (T1 `cast` sym (T CoX)) is a non-bottom value
of type (T X) constructed with T1.  Hence
	ANSWER = NO we can't prune the T1 branch (surprisingly)

Furthermore, this can even happen; see Trac #1251.  GHC's newtype-deriving
mechanism uses a cast, just as above, to move from one dictionary to another,
in effect giving the programmer access to CoX.

Finally, suppose x::T Y.  Then *even in FC* we can't construct a
non-bottom value of type (T Y) using T1.  That's because we can get
from Y to Char, but not to Int.


Here's a related question.  	data Eq a b where EQ :: Eq a a
Consider
	case x of { EQ -> ... }

Suppose x::Eq Int Char.  Is the alternative dead?  Clearly yes.

What about x::Eq Int a, in a context where we have evidence that a~Char.
Then again the alternative is dead.   


			Summary

We are really doing a test for unsatisfiability of the type
constraints implied by the match. And that is clearly, in general, a
hard thing to do.  

However, since we are simply dropping dead code, a conservative test
suffices.  There is a continuum of tests, ranging from easy to hard, that
drop more and more dead code.

For now we implement a very simple test: type variables match
anything, type functions (incl newtypes) match anything, and only
distinct data types fail to match.  We can elaborate later.

\begin{code}
typesCantMatch :: [(Type,Type)] -> Bool
typesCantMatch prs = any (\(s,t) -> cant_match s t) prs
  where
    cant_match :: Type -> Type -> Bool
    cant_match t1 t2
	| Just t1' <- coreView t1 = cant_match t1' t2
	| Just t2' <- coreView t2 = cant_match t1 t2'

    cant_match (FunTy a1 r1) (FunTy a2 r2)
	= cant_match a1 a2 || cant_match r1 r2

    cant_match (TyConApp tc1 tys1) (TyConApp tc2 tys2)
	| isDistinctTyCon tc1 && isDistinctTyCon tc2
	= tc1 /= tc2 || typesCantMatch (zipEqual "typesCantMatch" tys1 tys2)

    cant_match (FunTy {}) (TyConApp tc _) = isDistinctTyCon tc
    cant_match (TyConApp tc _) (FunTy {}) = isDistinctTyCon tc
	-- tc can't be FunTyCon by invariant

    cant_match (AppTy f1 a1) ty2
	| Just (f2, a2) <- repSplitAppTy_maybe ty2
	= cant_match f1 f2 || cant_match a1 a2
    cant_match ty1 (AppTy f2 a2)
	| Just (f1, a1) <- repSplitAppTy_maybe ty1
	= cant_match f1 f2 || cant_match a1 a2

    cant_match (LitTy x) (LitTy y) = x /= y

    cant_match _ _ = False      -- Safe!

-- Things we could add;
--	foralls
--	look through newtypes
--	take account of tyvar bindings (EQ example above)
\end{code}


%************************************************************************
%*									*
             Unification
%*                                                                      *
%************************************************************************

Note [Fine-grained unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do the types (x, x) and ([y], y) unify? The answer is seemingly "no" --
no substitution to finite types makes these match. But, a substitution to
*infinite* types can unify these two types: [x |-> [[[...]]], y |-> [[[...]]] ].
Why do we care? Consider these two type family instances:

type instance F x x   = Int
type instance F [y] y = Bool

If we also have

type instance Looper = [Looper]

then the instances potentially overlap. The solution is to use unification
over infinite terms. This is possible (see [1] for lots of gory details), but
a full algorithm is a little more power than we need. Instead, we make a
conservative approximation and just omit the occurs check.

[1]: http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/axioms-extended.pdf

tcUnifyTys considers an occurs-check problem as the same as general unification
failure.

tcUnifyTysFG ("fine-grained") returns one of three results: success, occurs-check
failure ("MaybeApart"), or general failure ("SurelyApart").

See also Trac #8162.

It's worth noting that unification in the presence of infinite types is not
complete. This means that, sometimes, a closed type family does not reduce
when it should. See test case indexed-types/should_fail/Overlap15 for an
example.

Note [The substitution in MaybeApart]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The constructor MaybeApart carries data with it, typically a TvSubstEnv. Why?
Because consider unifying these:

(a, a, Int) ~ (b, [b], Bool)

If we go left-to-right, we start with [a |-> b]. Then, on the middle terms, we
apply the subst we have so far and discover that we need [b |-> [b]]. Because
this fails the occurs check, we say that the types are MaybeApart (see above
Note [Fine-grained unification]). But, we can't stop there! Because if we
continue, we discover that Int is SurelyApart from Bool, and therefore the
types are apart. This has practical consequences for the ability for closed
type family applications to reduce. See test case
indexed-types/should_compile/Overlap14.

Note [Unifying with skolems]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we discover that two types unify if and only if a skolem variable is
substituted, we can't properly unify the types. But, that skolem variable
may later be instantiated with a unifyable type. So, we return maybeApart
in these cases.

\begin{code}
tcUnifyTy :: Type -> Type       -- All tyvars are bindable
	  -> Maybe TvSubst	-- A regular one-shot (idempotent) substitution
-- Simple unification of two types; all type variables are bindable
tcUnifyTy ty1 ty2
  = case initUM (const BindMe) (unify emptyTvSubstEnv ty1 ty2) of
      Unifiable subst_env -> Just (niFixTvSubst subst_env)
      _other              -> Nothing

-----------------
tcUnifyTys :: (TyVar -> BindFlag)
	   -> [Type] -> [Type]
	   -> Maybe TvSubst	-- A regular one-shot (idempotent) substitution
-- The two types may have common type variables, and indeed do so in the
-- second call to tcUnifyTys in FunDeps.checkClsFD
tcUnifyTys bind_fn tys1 tys2
  = case tcUnifyTysFG bind_fn tys1 tys2 of
      Unifiable subst -> Just subst
      _               -> Nothing

-- This type does double-duty. It is used in the UM (unifier monad) and to
-- return the final result. See Note [Fine-grained unification]
type UnifyResult = UnifyResultM TvSubst
data UnifyResultM a = Unifiable a        -- the subst that unifies the types
                    | MaybeApart a       -- the subst has as much as we know
                                         -- it must be part of an most general unifier
                                         -- See Note [The substitution in MaybeApart]
                    | SurelyApart

-- See Note [Fine-grained unification]
tcUnifyTysFG :: (TyVar -> BindFlag)
             -> [Type] -> [Type]
             -> UnifyResult
tcUnifyTysFG bind_fn tys1 tys2
  = initUM bind_fn $
    do { subst <- unifyList emptyTvSubstEnv tys1 tys2

	-- Find the fixed point of the resulting non-idempotent substitution
        ; return (niFixTvSubst subst) }
\end{code}


%************************************************************************
%*									*
                Non-idempotent substitution
%*									*
%************************************************************************

Note [Non-idempotent substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During unification we use a TvSubstEnv that is
  (a) non-idempotent
  (b) loop-free; ie repeatedly applying it yields a fixed point

Note [Finding the substitution fixpoint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Finding the fixpoint of a non-idempotent substitution arising from a
unification is harder than it looks, because of kinds.  Consider
   T k (H k (f:k)) ~ T * (g:*)
If we unify, we get the substitution
   [ k -> *
   , g -> H k (f:k) ]
To make it idempotent we don't want to get just
   [ k -> *
   , g -> H * (f:k) ]
We also want to substitute inside f's kind, to get
   [ k -> *
   , g -> H k (f:*) ]
If we don't do this, we may apply the substitition to something,
and get an ill-formed type, i.e. one where typeKind will fail.
This happened, for example, in Trac #9106.

This is the reason for extending env with [f:k -> f:*], in the
definition of env' in niFixTvSubst

\begin{code}
niFixTvSubst :: TvSubstEnv -> TvSubst
-- Find the idempotent fixed point of the non-idempotent substitution
-- See Note [Finding the substitution fixpoint]
-- ToDo: use laziness instead of iteration?
niFixTvSubst env = f env
  where
    f env | not_fixpoint = f (mapVarEnv (substTy subst') env)
          | otherwise    = subst
        where
          not_fixpoint  = foldVarSet ((||) . in_domain) False all_range_tvs
          in_domain tv  = tv `elemVarEnv` env

          range_tvs     = foldVarEnv (unionVarSet . tyVarsOfType) emptyVarSet env
          all_range_tvs = closeOverKinds range_tvs
          subst         = mkTvSubst (mkInScopeSet all_range_tvs) env

             -- env' extends env by replacing any free type with 
             -- that same tyvar with a substituted kind
             -- See note [Finding the substitution fixpoint]
          env'          = extendVarEnvList env [ (rtv, mkTyVarTy $ setTyVarKind rtv $
                                                       substTy subst $ tyVarKind rtv)
                                               | rtv <- varSetElems range_tvs
                                               , not (in_domain rtv) ]
          subst'        = mkTvSubst (mkInScopeSet all_range_tvs) env'

niSubstTvSet :: TvSubstEnv -> TyVarSet -> TyVarSet
-- Apply the non-idempotent substitution to a set of type variables,
-- remembering that the substitution isn't necessarily idempotent
-- This is used in the occurs check, before extending the substitution
niSubstTvSet subst tvs
  = foldVarSet (unionVarSet . get) emptyVarSet tvs
  where
    get tv = case lookupVarEnv subst tv of
	       Nothing -> unitVarSet tv
               Just ty -> niSubstTvSet subst (tyVarsOfType ty)
\end{code}

%************************************************************************
%*									*
		The workhorse
%*									*
%************************************************************************

\begin{code}
unify :: TvSubstEnv	-- An existing substitution to extend
      -> Type -> Type 	-- Types to be unified, and witness of their equality
      -> UM TvSubstEnv		-- Just the extended substitution, 
				-- Nothing if unification failed
-- We do not require the incoming substitution to be idempotent,
-- nor guarantee that the outgoing one is.  That's fixed up by
-- the wrappers.

-- Respects newtypes, PredTypes

-- in unify, any NewTcApps/Preds should be taken at face value
unify subst (TyVarTy tv1) ty2  = uVar subst tv1 ty2
unify subst ty1 (TyVarTy tv2)  = uVar subst tv2 ty1

unify subst ty1 ty2 | Just ty1' <- tcView ty1 = unify subst ty1' ty2
unify subst ty1 ty2 | Just ty2' <- tcView ty2 = unify subst ty1 ty2'

unify subst (TyConApp tyc1 tys1) (TyConApp tyc2 tys2) 
  | tyc1 == tyc2                                   
  = unify_tys subst tys1 tys2

unify subst (FunTy ty1a ty1b) (FunTy ty2a ty2b) 
  = do	{ subst' <- unify subst ty1a ty2a
	; unify subst' ty1b ty2b }

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
unify subst (AppTy ty1a ty1b) ty2
  | Just (ty2a, ty2b) <- repSplitAppTy_maybe ty2
  = do	{ subst' <- unify subst ty1a ty2a
        ; unify subst' ty1b ty2b }

unify subst ty1 (AppTy ty2a ty2b)
  | Just (ty1a, ty1b) <- repSplitAppTy_maybe ty1
  = do	{ subst' <- unify subst ty1a ty2a
        ; unify subst' ty1b ty2b }

unify subst (LitTy x) (LitTy y) | x == y = return subst

unify _ _ _ = surelyApart
	-- ForAlls??

------------------------------
unify_tys :: TvSubstEnv -> [Type] -> [Type] -> UM TvSubstEnv
unify_tys subst xs ys = unifyList subst xs ys

unifyList :: TvSubstEnv -> [Type] -> [Type] -> UM TvSubstEnv
unifyList subst orig_xs orig_ys
  = go subst orig_xs orig_ys
  where
    go subst []     []     = return subst
    go subst (x:xs) (y:ys) = do { subst' <- unify subst x y
				; go subst' xs ys }
    go _ _ _ = surelyApart

---------------------------------
uVar :: TvSubstEnv	-- An existing substitution to extend
     -> TyVar           -- Type variable to be unified
     -> Type            -- with this type
     -> UM TvSubstEnv

uVar subst tv1 ty
 = -- Check to see whether tv1 is refined by the substitution
   case (lookupVarEnv subst tv1) of
     Just ty' -> unify subst ty' ty     -- Yes, call back into unify'
     Nothing  -> uUnrefined subst       -- No, continue
			    tv1 ty ty

uUnrefined :: TvSubstEnv          -- An existing substitution to extend
           -> TyVar               -- Type variable to be unified
           -> Type                -- with this type
           -> Type                -- (version w/ expanded synonyms)
           -> UM TvSubstEnv

-- We know that tv1 isn't refined

uUnrefined subst tv1 ty2 ty2'
  | Just ty2'' <- tcView ty2'
  = uUnrefined subst tv1 ty2 ty2''	-- Unwrap synonyms
		-- This is essential, in case we have
		--	type Foo a = a
		-- and then unify a ~ Foo a

uUnrefined subst tv1 ty2 (TyVarTy tv2)
  | tv1 == tv2		-- Same type variable
  = return subst

    -- Check to see whether tv2 is refined
  | Just ty' <- lookupVarEnv subst tv2
  = uUnrefined subst tv1 ty' ty'

  | otherwise

  = do {   -- So both are unrefined; unify the kinds
       ; subst' <- unify subst (tyVarKind tv1) (tyVarKind tv2)

           -- And then bind one or the other, 
           -- depending on which is bindable
	   -- NB: unlike TcUnify we do not have an elaborate sub-kinding 
	   --     story.  That is relevant only during type inference, and
           --     (I very much hope) is not relevant here.
       ; b1 <- tvBindFlag tv1
       ; b2 <- tvBindFlag tv2
       ; let ty1 = TyVarTy tv1
       ; case (b1, b2) of
           (Skolem, Skolem) -> maybeApart subst' -- See Note [Unification with skolems]
           (BindMe, _)      -> return (extendVarEnv subst' tv1 ty2)
           (_, BindMe)      -> return (extendVarEnv subst' tv2 ty1) }

uUnrefined subst tv1 ty2 ty2'	-- ty2 is not a type variable
  | tv1 `elemVarSet` niSubstTvSet subst (tyVarsOfType ty2')
  = maybeApart subst                    -- Occurs check
                                        -- See Note [Fine-grained unification]
  | otherwise
  = do { subst' <- unify subst k1 k2
       -- Note [Kinds Containing Only Literals]
       ; let ki = substTy (mkOpenTvSubst subst') k1
       ; unless (validKindShape ki ty2')
           surelyApart
       ; bindTv subst' tv1 ty2 }	-- Bind tyvar to the synonym if poss
  where
    k1 = tyVarKind tv1
    k2 = typeKind ty2'

bindTv :: TvSubstEnv -> TyVar -> Type -> UM TvSubstEnv
bindTv subst tv ty	-- ty is not a type variable
  = do  { b <- tvBindFlag tv
	; case b of
	    Skolem -> maybeApart subst -- See Note [Unification with skolems]
	    BindMe -> return $ extendVarEnv subst tv ty
	}
\end{code}

%************************************************************************
%*									*
		Binding decisions
%*									*
%************************************************************************

\begin{code}
data BindFlag 
  = BindMe	-- A regular type variable

  | Skolem	-- This type variable is a skolem constant
		-- Don't bind it; it only matches itself
\end{code}


%************************************************************************
%*									*
		Unification monad
%*									*
%************************************************************************

\begin{code}
newtype UM a = UM { unUM :: (TyVar -> BindFlag)
		         -> UnifyResultM a }

instance Functor UM where
      fmap = liftM

instance Applicative UM where
      pure = return
      (<*>) = ap

instance Monad UM where
  return a = UM (\_tvs -> Unifiable a)
  fail _   = UM (\_tvs -> SurelyApart) -- failed pattern match
  m >>= k  = UM (\tvs -> case unUM m tvs of
			   Unifiable v -> unUM (k v) tvs
                           MaybeApart v ->
                             case unUM (k v) tvs of
                               Unifiable v' -> MaybeApart v'
                               other        -> other
                           SurelyApart -> SurelyApart)

initUM :: (TyVar -> BindFlag) -> UM a -> UnifyResultM a
initUM badtvs um = unUM um badtvs

tvBindFlag :: TyVar -> UM BindFlag
tvBindFlag tv = UM (\tv_fn -> Unifiable (tv_fn tv))

maybeApart :: TvSubstEnv -> UM TvSubstEnv
maybeApart subst = UM (\_tv_fn -> MaybeApart subst)

surelyApart :: UM a
surelyApart = UM (\_tv_fn -> SurelyApart)
\end{code}

