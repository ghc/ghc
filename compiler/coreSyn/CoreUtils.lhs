%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Utility functions on @Core@ syntax

\begin{code}
-- | Commonly useful utilites for manipulating the Core language
module CoreUtils (
        -- * Constructing expressions
        mkCast,
        mkTick, mkTickNoHNF, tickHNFArgs,
        bindNonRec, needsCaseBinding,
        mkAltExpr,

        -- * Taking expressions apart
        findDefault, findAlt, isDefaultAlt,
        mergeAlts, trimConArgs, filterAlts,

        -- * Properties of expressions
        exprType, coreAltType, coreAltsType,
        exprIsDupable, exprIsTrivial, getIdFromTrivialExpr, exprIsBottom,
        exprIsCheap, exprIsExpandable, exprIsCheap', CheapAppFun,
        exprIsHNF, exprOkForSpeculation, exprOkForSideEffects, exprIsWorkFree,
        exprIsBig, exprIsConLike,
        rhsIsStatic, isCheapApp, isExpandableApp,

        -- * Expression and bindings size
        coreBindsSize, exprSize,
        CoreStats(..), coreBindsStats,

        -- * Hashing
        hashExpr,

        -- * Equality
        cheapEqExpr, eqExpr, eqExprX,

        -- * Eta reduction
        tryEtaReduce,

        -- * Manipulating data constructors and types
        applyTypeToArgs, applyTypeToArg,
        dataConRepInstPat, dataConRepFSInstPat
    ) where

#include "HsVersions.h"

import CoreSyn
import PprCore
import Var
import SrcLoc
import VarEnv
import VarSet
import Name
import Literal
import DataCon
import PrimOp
import Id
import IdInfo
import Type
import Coercion
import TyCon
import Unique
import Outputable
import TysPrim
import DynFlags
import FastString
import Maybes
import Platform
import Util
import Pair
import Data.Word
import Data.Bits
import Data.List
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Find the type of a Core atom/expression}
%*                                                                      *
%************************************************************************

\begin{code}
exprType :: CoreExpr -> Type
-- ^ Recover the type of a well-typed Core expression. Fails when
-- applied to the actual 'CoreSyn.Type' expression as it cannot
-- really be said to have a type
exprType (Var var)           = idType var
exprType (Lit lit)           = literalType lit
exprType (Coercion co)       = coercionType co
exprType (Let _ body)        = exprType body
exprType (Case _ _ ty _)     = ty
exprType (Cast _ co)         = pSnd (coercionKind co)
exprType (Tick _ e)          = exprType e
exprType (Lam binder expr)   = mkPiType binder (exprType expr)
exprType e@(App _ _)
  = case collectArgs e of
        (fun, args) -> applyTypeToArgs e (exprType fun) args

exprType other = pprTrace "exprType" (pprCoreExpr other) alphaTy

coreAltType :: CoreAlt -> Type
-- ^ Returns the type of the alternatives right hand side
coreAltType (_,bs,rhs)
  | any bad_binder bs = expandTypeSynonyms ty
  | otherwise         = ty    -- Note [Existential variables and silly type synonyms]
  where
    ty           = exprType rhs
    free_tvs     = tyVarsOfType ty
    bad_binder b = isTyVar b && b `elemVarSet` free_tvs

coreAltsType :: [CoreAlt] -> Type
-- ^ Returns the type of the first alternative, which should be the same as for all alternatives
coreAltsType (alt:_) = coreAltType alt
coreAltsType []      = panic "corAltsType"
\end{code}

Note [Existential variables and silly type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        data T = forall a. T (Funny a)
        type Funny a = Bool
        f :: T -> Bool
        f (T x) = x

Now, the type of 'x' is (Funny a), where 'a' is existentially quantified.
That means that 'exprType' and 'coreAltsType' may give a result that *appears*
to mention an out-of-scope type variable.  See Trac #3409 for a more real-world
example.

Various possibilities suggest themselves:

 - Ignore the problem, and make Lint not complain about such variables

 - Expand all type synonyms (or at least all those that discard arguments)
      This is tricky, because at least for top-level things we want to
      retain the type the user originally specified.

 - Expand synonyms on the fly, when the problem arises. That is what
   we are doing here.  It's not too expensive, I think.

\begin{code}
applyTypeToArg :: Type -> CoreExpr -> Type
-- ^ Determines the type resulting from applying an expression to a function with the given type
applyTypeToArg fun_ty (Type arg_ty) = applyTy fun_ty arg_ty
applyTypeToArg fun_ty _             = funResultTy fun_ty

applyTypeToArgs :: CoreExpr -> Type -> [CoreExpr] -> Type
-- ^ A more efficient version of 'applyTypeToArg' when we have several arguments.
-- The first argument is just for debugging, and gives some context
applyTypeToArgs _ op_ty [] = op_ty

applyTypeToArgs e op_ty (Type ty : args)
  =     -- Accumulate type arguments so we can instantiate all at once
    go [ty] args
  where
    go rev_tys (Type ty : args) = go (ty:rev_tys) args
    go rev_tys rest_args         = applyTypeToArgs e op_ty' rest_args
                                 where
                                   op_ty' = applyTysD msg op_ty (reverse rev_tys)
                                   msg = ptext (sLit "applyTypeToArgs") <+>
                                         panic_msg e op_ty

applyTypeToArgs e op_ty (_ : args)
  = case (splitFunTy_maybe op_ty) of
        Just (_, res_ty) -> applyTypeToArgs e res_ty args
        Nothing -> pprPanic "applyTypeToArgs" (panic_msg e op_ty)

panic_msg :: CoreExpr -> Type -> SDoc
panic_msg e op_ty = pprCoreExpr e $$ ppr op_ty
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Attaching notes}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Wrap the given expression in the coercion safely, dropping
-- identity coercions and coalescing nested coercions
mkCast :: CoreExpr -> Coercion -> CoreExpr
mkCast e co | isReflCo co = e

mkCast (Coercion e_co) co 
  | isCoVarType (pSnd (coercionKind co))
       -- The guard here checks that g has a (~#) on both sides,
       -- otherwise decomposeCo fails.  Can in principle happen
       -- with unsafeCoerce
  = Coercion (mkCoCast e_co co)

mkCast (Cast expr co2) co
  = ASSERT(let { Pair  from_ty  _to_ty  = coercionKind co;
                 Pair _from_ty2  to_ty2 = coercionKind co2} in
           from_ty `eqType` to_ty2 )
    mkCast expr (mkTransCo co2 co)

mkCast expr co
  = let Pair from_ty _to_ty = coercionKind co in
--    if to_ty `eqType` from_ty
--    then expr
--    else
        WARN(not (from_ty `eqType` exprType expr), text "Trying to coerce" <+> text "(" <> ppr expr $$ text "::" <+> ppr (exprType expr) <> text ")" $$ ppr co $$ pprEqPred (coercionKind co))
         (Cast expr co)
\end{code}

\begin{code}
-- | Wraps the given expression in the source annotation, dropping the
-- annotation if possible.
mkTick :: Tickish Id -> CoreExpr -> CoreExpr

mkTick t (Var x)
  | isFunTy (idType x) = Tick t (Var x)
  | otherwise
  = if tickishCounts t
       then if tickishScoped t && tickishCanSplit t
               then Tick (mkNoScope t) (Var x)
               else Tick t (Var x)
       else Var x

mkTick t (Cast e co)
  = Cast (mkTick t e) co -- Move tick inside cast

mkTick _ (Coercion co) = Coercion co

mkTick t (Lit l)
  | not (tickishCounts t) = Lit l

mkTick t expr@(App f arg)
  | not (isRuntimeArg arg) = App (mkTick t f) arg
  | isSaturatedConApp expr
    = if not (tickishCounts t)
         then tickHNFArgs t expr
         else if tickishScoped t && tickishCanSplit t
                 then Tick (mkNoScope t) (tickHNFArgs (mkNoTick t) expr)
                 else Tick t expr

mkTick t (Lam x e)
     -- if this is a type lambda, or the tick does not count entries,
     -- then we can push the tick inside:
  | not (isRuntimeVar x) || not (tickishCounts t) = Lam x (mkTick t e)
     -- if it is both counting and scoped, we split the tick into its
     -- two components, keep the counting tick on the outside of the lambda
     -- and push the scoped tick inside.  The point of this is that the
     -- counting tick can probably be floated, and the lambda may then be
     -- in a position to be beta-reduced.
  | tickishScoped t && tickishCanSplit t
         = Tick (mkNoScope t) (Lam x (mkTick (mkNoTick t) e))
     -- just a counting tick: leave it on the outside
  | otherwise        = Tick t (Lam x e)

mkTick t other = Tick t other

isSaturatedConApp :: CoreExpr -> Bool
isSaturatedConApp e = go e []
  where go (App f a) as = go f (a:as)
        go (Var fun) args
           = isConLikeId fun && idArity fun == valArgCount args
        go (Cast f _) as = go f as
        go _ _ = False

mkTickNoHNF :: Tickish Id -> CoreExpr -> CoreExpr
mkTickNoHNF t e
  | exprIsHNF e = tickHNFArgs t e
  | otherwise   = mkTick t e

-- push a tick into the arguments of a HNF (call or constructor app)
tickHNFArgs :: Tickish Id -> CoreExpr -> CoreExpr
tickHNFArgs t e = push t e
 where
  push t (App f (Type u)) = App (push t f) (Type u)
  push t (App f arg) = App (push t f) (mkTick t arg)
  push _t e = e
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Other expression construction}
%*                                                                      *
%************************************************************************

\begin{code}
bindNonRec :: Id -> CoreExpr -> CoreExpr -> CoreExpr
-- ^ @bindNonRec x r b@ produces either:
--
-- > let x = r in b
--
-- or:
--
-- > case r of x { _DEFAULT_ -> b }
--
-- depending on whether we have to use a @case@ or @let@
-- binding for the expression (see 'needsCaseBinding').
-- It's used by the desugarer to avoid building bindings
-- that give Core Lint a heart attack, although actually
-- the simplifier deals with them perfectly well. See
-- also 'MkCore.mkCoreLet'
bindNonRec bndr rhs body
  | needsCaseBinding (idType bndr) rhs = Case rhs bndr (exprType body) [(DEFAULT, [], body)]
  | otherwise                          = Let (NonRec bndr rhs) body

-- | Tests whether we have to use a @case@ rather than @let@ binding for this expression
-- as per the invariants of 'CoreExpr': see "CoreSyn#let_app_invariant"
needsCaseBinding :: Type -> CoreExpr -> Bool
needsCaseBinding ty rhs = isUnLiftedType ty && not (exprOkForSpeculation rhs)
        -- Make a case expression instead of a let
        -- These can arise either from the desugarer,
        -- or from beta reductions: (\x.e) (x +# y)
\end{code}

\begin{code}
mkAltExpr :: AltCon     -- ^ Case alternative constructor
          -> [CoreBndr] -- ^ Things bound by the pattern match
          -> [Type]     -- ^ The type arguments to the case alternative
          -> CoreExpr
-- ^ This guy constructs the value that the scrutinee must have
-- given that you are in one particular branch of a case
mkAltExpr (DataAlt con) args inst_tys
  = mkConApp con (map Type inst_tys ++ varsToCoreExprs args)
mkAltExpr (LitAlt lit) [] []
  = Lit lit
mkAltExpr (LitAlt _) _ _ = panic "mkAltExpr LitAlt"
mkAltExpr DEFAULT _ _ = panic "mkAltExpr DEFAULT"
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Taking expressions apart}
%*                                                                      *
%************************************************************************

The default alternative must be first, if it exists at all.
This makes it easy to find, though it makes matching marginally harder.

\begin{code}
-- | Extract the default case alternative
findDefault :: [(AltCon, [a], b)] -> ([(AltCon, [a], b)], Maybe b)
findDefault ((DEFAULT,args,rhs) : alts) = ASSERT( null args ) (alts, Just rhs)
findDefault alts                        =                     (alts, Nothing)

isDefaultAlt :: (AltCon, a, b) -> Bool
isDefaultAlt (DEFAULT, _, _) = True
isDefaultAlt _               = False


-- | Find the case alternative corresponding to a particular
-- constructor: panics if no such constructor exists
findAlt :: AltCon -> [(AltCon, a, b)] -> Maybe (AltCon, a, b)
    -- A "Nothing" result *is* legitmiate
    -- See Note [Unreachable code]
findAlt con alts
  = case alts of
        (deflt@(DEFAULT,_,_):alts) -> go alts (Just deflt)
        _                          -> go alts Nothing
  where
    go []                     deflt = deflt
    go (alt@(con1,_,_) : alts) deflt
      = case con `cmpAltCon` con1 of
          LT -> deflt   -- Missed it already; the alts are in increasing order
          EQ -> Just alt
          GT -> ASSERT( not (con1 == DEFAULT) ) go alts deflt

---------------------------------
mergeAlts :: [(AltCon, a, b)] -> [(AltCon, a, b)] -> [(AltCon, a, b)]
-- ^ Merge alternatives preserving order; alternatives in
-- the first argument shadow ones in the second
mergeAlts [] as2 = as2
mergeAlts as1 [] = as1
mergeAlts (a1:as1) (a2:as2)
  = case a1 `cmpAlt` a2 of
        LT -> a1 : mergeAlts as1      (a2:as2)
        EQ -> a1 : mergeAlts as1      as2       -- Discard a2
        GT -> a2 : mergeAlts (a1:as1) as2


---------------------------------
trimConArgs :: AltCon -> [CoreArg] -> [CoreArg]
-- ^ Given:
--
-- > case (C a b x y) of
-- >        C b x y -> ...
--
-- We want to drop the leading type argument of the scrutinee
-- leaving the arguments to match agains the pattern

trimConArgs DEFAULT      args = ASSERT( null args ) []
trimConArgs (LitAlt _)   args = ASSERT( null args ) []
trimConArgs (DataAlt dc) args = dropList (dataConUnivTyVars dc) args
\end{code}

\begin{code}
filterAlts :: [Unique]             -- ^ Supply of uniques used in case we have to manufacture a new AltCon
           -> Type                 -- ^ Type of scrutinee (used to prune possibilities)
           -> [AltCon]             -- ^ 'imposs_cons': constructors known to be impossible due to the form of the scrutinee
           -> [(AltCon, [Var], a)] -- ^ Alternatives
           -> ([AltCon], Bool, [(AltCon, [Var], a)])
             -- Returns:
             --  1. Constructors that will never be encountered by the 
             --     *default* case (if any).  A superset of imposs_cons
             --  2. Whether we managed to refine the default alternative into a specific constructor (for statistics only)
             --  3. The new alternatives, trimmed by
             --        a) remove imposs_cons
             --        b) remove constructors which can't match because of GADTs
             --      and with the DEFAULT expanded to a DataAlt if there is exactly
             --      remaining constructor that can match
             --
             -- NB: the final list of alternatives may be empty:
             -- This is a tricky corner case.  If the data type has no constructors,
             -- which GHC allows, or if the imposs_cons covers all constructors (after taking 
             -- account of GADTs), then no alternatives can match.
             --
             -- If callers need to preserve the invariant that there is always at least one branch
             -- in a "case" statement then they will need to manually add a dummy case branch that just
             -- calls "error" or similar.
filterAlts us ty imposs_cons alts 
  | Just (tycon, inst_tys) <- splitTyConApp_maybe ty
  = filter_alts tycon inst_tys
  | otherwise
  = (imposs_cons, False, alts)
  where
    (alts_wo_default, maybe_deflt) = findDefault alts
    alt_cons = [con | (con,_,_) <- alts_wo_default]

    filter_alts tycon inst_tys 
      = (imposs_deflt_cons, refined_deflt, merged_alts)
     where
       trimmed_alts = filterOut (impossible_alt inst_tys) alts_wo_default

       imposs_deflt_cons = nub (imposs_cons ++ alt_cons)
         -- "imposs_deflt_cons" are handled 
         --   EITHER by the context, 
         --   OR by a non-DEFAULT branch in this case expression.

       merged_alts  = mergeAlts trimmed_alts (maybeToList maybe_deflt')
         -- We need the mergeAlts in case the new default_alt 
         -- has turned into a constructor alternative.
         -- The merge keeps the inner DEFAULT at the front, if there is one
         -- and interleaves the alternatives in the right order

       (refined_deflt, maybe_deflt') = case maybe_deflt of
          Nothing -> (False, Nothing)
          Just deflt_rhs 
             | isAlgTyCon tycon            -- It's a data type, tuple, or unboxed tuples.  
             , not (isNewTyCon tycon)      -- We can have a newtype, if we are just doing an eval:
                                           --      case x of { DEFAULT -> e }
                                           -- and we don't want to fill in a default for them!
             , Just all_cons <- tyConDataCons_maybe tycon
             , let imposs_data_cons = [con | DataAlt con <- imposs_deflt_cons]   -- We now know it's a data type 
                   impossible con   = con `elem` imposs_data_cons || dataConCannotMatch inst_tys con
             -> case filterOut impossible all_cons of
                  -- Eliminate the default alternative
                  -- altogether if it can't match:
                  []    -> (False, Nothing)
                  -- It matches exactly one constructor, so fill it in:
                  [con] -> (True, Just (DataAlt con, ex_tvs ++ arg_ids, deflt_rhs))
                    where (ex_tvs, arg_ids) = dataConRepInstPat us con inst_tys
                  _     -> (False, Just (DEFAULT, [], deflt_rhs))

             | debugIsOn, isAlgTyCon tycon
             , null (tyConDataCons tycon)
             , not (isFamilyTyCon tycon || isAbstractTyCon tycon)
                   -- Check for no data constructors
                   -- This can legitimately happen for abstract types and type families,
                   -- so don't report that
             -> pprTrace "prepareDefault" (ppr tycon)
                (False, Just (DEFAULT, [], deflt_rhs))

             | otherwise -> (False, Just (DEFAULT, [], deflt_rhs))

    impossible_alt :: [Type] -> (AltCon, a, b) -> Bool
    impossible_alt _ (con, _, _) | con `elem` imposs_cons = True
    impossible_alt inst_tys (DataAlt con, _, _) = dataConCannotMatch inst_tys con
    impossible_alt _  _                         = False
\end{code}

Note [Unreachable code]
~~~~~~~~~~~~~~~~~~~~~~~
It is possible (although unusual) for GHC to find a case expression
that cannot match.  For example:

     data Col = Red | Green | Blue
     x = Red
     f v = case x of
              Red -> ...
              _ -> ...(case x of { Green -> e1; Blue -> e2 })...

Suppose that for some silly reason, x isn't substituted in the case
expression.  (Perhaps there's a NOINLINE on it, or profiling SCC stuff
gets in the way; cf Trac #3118.)  Then the full-lazines pass might produce
this

     x = Red
     lvl = case x of { Green -> e1; Blue -> e2 })
     f v = case x of
             Red -> ...
             _ -> ...lvl...

Now if x gets inlined, we won't be able to find a matching alternative
for 'Red'.  That's because 'lvl' is unreachable.  So rather than crashing
we generate (error "Inaccessible alternative").

Similar things can happen (augmented by GADTs) when the Simplifier
filters down the matching alternatives in Simplify.rebuildCase.


%************************************************************************
%*                                                                      *
             exprIsTrivial
%*                                                                      *
%************************************************************************

Note [exprIsTrivial]
~~~~~~~~~~~~~~~~~~~~
@exprIsTrivial@ is true of expressions we are unconditionally happy to
                duplicate; simple variables and constants, and type
                applications.  Note that primop Ids aren't considered
                trivial unless

Note [Variable are trivial]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
There used to be a gruesome test for (hasNoBinding v) in the
Var case:
        exprIsTrivial (Var v) | hasNoBinding v = idArity v == 0
The idea here is that a constructor worker, like \$wJust, is
really short for (\x -> \$wJust x), becuase \$wJust has no binding.
So it should be treated like a lambda.  Ditto unsaturated primops.
But now constructor workers are not "have-no-binding" Ids.  And
completely un-applied primops and foreign-call Ids are sufficiently
rare that I plan to allow them to be duplicated and put up with
saturating them.

Note [Tick trivial]
~~~~~~~~~~~~~~~~~~~
Ticks are not trivial.  If we treat "tick<n> x" as trivial, it will be
inlined inside lambdas and the entry count will be skewed, for
example.  Furthermore "scc<n> x" will turn into just "x" in mkTick.

\begin{code}
exprIsTrivial :: CoreExpr -> Bool
exprIsTrivial (Var _)          = True        -- See Note [Variables are trivial]
exprIsTrivial (Type _)        = True
exprIsTrivial (Coercion _)     = True
exprIsTrivial (Lit lit)        = litIsTrivial lit
exprIsTrivial (App e arg)      = not (isRuntimeArg arg) && exprIsTrivial e
exprIsTrivial (Tick _ _)       = False  -- See Note [Tick trivial]
exprIsTrivial (Cast e _)       = exprIsTrivial e
exprIsTrivial (Lam b body)     = not (isRuntimeVar b) && exprIsTrivial body
exprIsTrivial _                = False
\end{code}

When substituting in a breakpoint we need to strip away the type cruft
from a trivial expression and get back to the Id.  The invariant is
that the expression we're substituting was originally trivial
according to exprIsTrivial.

\begin{code}
getIdFromTrivialExpr :: CoreExpr -> Id
getIdFromTrivialExpr e = go e
  where go (Var v) = v
        go (App f t) | not (isRuntimeArg t) = go f
        go (Cast e _) = go e
        go (Lam b e) | not (isRuntimeVar b) = go e
        go e = pprPanic "getIdFromTrivialExpr" (ppr e)
\end{code}

exprIsBottom is a very cheap and cheerful function; it may return
False for bottoming expressions, but it never costs much to ask.
See also CoreArity.exprBotStrictness_maybe, but that's a bit more
expensive.

\begin{code}
exprIsBottom :: CoreExpr -> Bool
exprIsBottom e
  = go 0 e
  where
    go n (Var v) = isBottomingId v &&  n >= idArity v
    go n (App e a) | isTypeArg a = go n e
                   | otherwise   = go (n+1) e
    go n (Tick _ e)              = go n e
    go n (Cast e _)              = go n e
    go n (Let _ e)               = go n e
    go _ _                       = False
\end{code}


%************************************************************************
%*                                                                      *
             exprIsDupable
%*                                                                      *
%************************************************************************

Note [exprIsDupable]
~~~~~~~~~~~~~~~~~~~~
@exprIsDupable@ is true of expressions that can be duplicated at a modest
                cost in code size.  This will only happen in different case
                branches, so there's no issue about duplicating work.

                That is, exprIsDupable returns True of (f x) even if
                f is very very expensive to call.

                Its only purpose is to avoid fruitless let-binding
                and then inlining of case join points


\begin{code}
exprIsDupable :: DynFlags -> CoreExpr -> Bool
exprIsDupable dflags e
  = isJust (go dupAppSize e)
  where
    go :: Int -> CoreExpr -> Maybe Int
    go n (Type {})     = Just n
    go n (Coercion {}) = Just n
    go n (Var {})      = decrement n
    go n (Tick _ e)    = go n e
    go n (Cast e _)    = go n e
    go n (App f a) | Just n' <- go n a = go n' f
    go n (Lit lit) | litIsDupable dflags lit = decrement n
    go _ _ = Nothing

    decrement :: Int -> Maybe Int
    decrement 0 = Nothing
    decrement n = Just (n-1)

dupAppSize :: Int
dupAppSize = 8   -- Size of term we are prepared to duplicate
                 -- This is *just* big enough to make test MethSharing
                 -- inline enough join points.  Really it should be
                 -- smaller, and could be if we fixed Trac #4960.
\end{code}

%************************************************************************
%*                                                                      *
             exprIsCheap, exprIsExpandable
%*                                                                      *
%************************************************************************

Note [exprIsWorkFree]
~~~~~~~~~~~~~~~~~~~~~
exprIsWorkFree is used when deciding whether to inline something; we
don't inline it if doing so might duplicate work, by peeling off a
complete copy of the expression.  Here we do not want even to
duplicate a primop (Trac #5623):
   eg   let x = a #+ b in x +# x
   we do not want to inline/duplicate x

Previously we were a bit more liberal, which led to the primop-duplicating
problem.  However, being more conservative did lead to a big regression in
one nofib benchmark, wheel-sieve1.  The situation looks like this:

   let noFactor_sZ3 :: GHC.Types.Int -> GHC.Types.Bool
       noFactor_sZ3 = case s_adJ of _ { GHC.Types.I# x_aRs ->
         case GHC.Prim.<=# x_aRs 2 of _ {
           GHC.Types.False -> notDivBy ps_adM qs_adN;
           GHC.Types.True -> lvl_r2Eb }}
       go = \x. ...(noFactor (I# y))....(go x')...

The function 'noFactor' is heap-allocated and then called.  Turns out
that 'notDivBy' is strict in its THIRD arg, but that is invisible to
the caller of noFactor, which therefore cannot do w/w and
heap-allocates noFactor's argument.  At the moment (May 12) we are just
going to put up with this, because the previous more aggressive inlining 
(which treated 'noFactor' as work-free) was duplicating primops, which 
in turn was making inner loops of array calculations runs slow (#5623)

\begin{code}
exprIsWorkFree :: CoreExpr -> Bool
-- See Note [exprIsWorkFree]
exprIsWorkFree e = go 0 e
  where    -- n is the number of value arguments
    go _ (Lit {})                     = True
    go _ (Type {})                    = True
    go _ (Coercion {})                = True
    go n (Cast e _)                   = go n e
    go n (Case scrut _ _ alts)        = foldl (&&) (exprIsWorkFree scrut) 
                                              [ go n rhs | (_,_,rhs) <- alts ]
         -- See Note [Case expressions are work-free]
    go _ (Let {})                     = False
    go n (Var v)                      = isCheapApp v n
    go n (Tick t e) | tickishCounts t = False
                    | otherwise       = go n e
    go n (Lam x e)  | isRuntimeVar x = n==0 || go (n-1) e
                    | otherwise      = go n e
    go n (App f e)  | isRuntimeArg e = exprIsWorkFree e && go (n+1) f
                    | otherwise      = go n f
\end{code}

Note [Case expressions are work-free]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Are case-expressions work-free?  Consider
    let v = case x of (p,q) -> p
        go = \y -> ...case v of ...
Should we inline 'v' at its use site inside the loop?  At the moment
we do.  I experimented with saying that case are *not* work-free, but
that increased allocation slightly.  It's a fairly small effect, and at
the moment we go for the slightly more aggressive version which treats
(case x of ....) as work-free if the alterantives are.


Note [exprIsCheap]   See also Note [Interaction of exprIsCheap and lone variables]
~~~~~~~~~~~~~~~~~~   in CoreUnfold.lhs
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

  *     case e of
          pi -> ei
        (where e, and all the ei are cheap)

  *     let x = e in b
        (where e and b are cheap)

  *     op x1 ... xn
        (where op is a cheap primitive operator)

  *     error "foo"
        (because we are happy to substitute it inside a lambda)

Notice that a variable is considered 'cheap': we can push it inside a lambda,
because sharing will make sure it is only evaluated once.

Note [exprIsCheap and exprIsHNF]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that exprIsHNF does not imply exprIsCheap.  Eg
        let x = fac 20 in Just x
This responds True to exprIsHNF (you can discard a seq), but
False to exprIsCheap.

\begin{code}
exprIsCheap :: CoreExpr -> Bool
exprIsCheap = exprIsCheap' isCheapApp

exprIsExpandable :: CoreExpr -> Bool
exprIsExpandable = exprIsCheap' isExpandableApp -- See Note [CONLIKE pragma] in BasicTypes

exprIsCheap' :: CheapAppFun -> CoreExpr -> Bool
exprIsCheap' _        (Lit _)      = True
exprIsCheap' _        (Type _)    = True
exprIsCheap' _        (Coercion _) = True
exprIsCheap' _        (Var _)      = True
exprIsCheap' good_app (Cast e _)   = exprIsCheap' good_app e
exprIsCheap' good_app (Lam x e)    = isRuntimeVar x
                                  || exprIsCheap' good_app e

exprIsCheap' good_app (Case e _ _ alts) = exprIsCheap' good_app e &&
                                          and [exprIsCheap' good_app rhs | (_,_,rhs) <- alts]
        -- Experimentally, treat (case x of ...) as cheap
        -- (and case __coerce x etc.)
        -- This improves arities of overloaded functions where
        -- there is only dictionary selection (no construction) involved

exprIsCheap' good_app (Tick t e)
  | tickishCounts t = False
  | otherwise       = exprIsCheap' good_app e
     -- never duplicate ticks.  If we get this wrong, then HPC's entry
     -- counts will be off (check test in libraries/hpc/tests/raytrace)

exprIsCheap' good_app (Let (NonRec x _) e)
  | isUnLiftedType (idType x) = exprIsCheap' good_app e
  | otherwise                 = False
        -- Strict lets always have cheap right hand sides,
        -- and do no allocation, so just look at the body
        -- Non-strict lets do allocation so we don't treat them as cheap
        -- See also

exprIsCheap' good_app other_expr        -- Applications and variables
  = go other_expr []
  where
        -- Accumulate value arguments, then decide
    go (Cast e _) val_args                 = go e val_args
    go (App f a) val_args | isRuntimeArg a = go f (a:val_args)
                          | otherwise      = go f val_args

    go (Var _) [] = True        
         -- Just a type application of a variable
         -- (f t1 t2 t3) counts as WHNF
         -- This case is probably handeld by the good_app case
         -- below, which should have a case for n=0, but putting
         -- it here too is belt and braces; and it's such a common
         -- case that checking for null directly seems like a 
         -- good plan

    go (Var f) args
       | good_app f (length args) 
       = go_pap args

       | otherwise
        = case idDetails f of
                RecSelId {}         -> go_sel args
                ClassOpId {}        -> go_sel args
                PrimOpId op         -> go_primop op args
                _ | isBottomingId f -> True
                  | otherwise       -> False
                        -- Application of a function which
                        -- always gives bottom; we treat this as cheap
                        -- because it certainly doesn't need to be shared!

    go _ _ = False

    --------------
    go_pap args = all (exprIsCheap' good_app) args
        -- Used to be "all exprIsTrivial args" due to concerns about
        -- duplicating nested constructor applications, but see #4978.
        -- The principle here is that
        --    let x = a +# b in c *# x
        -- should behave equivalently to
        --    c *# (a +# b)
        -- Since lets with cheap RHSs are accepted,
        -- so should paps with cheap arguments

    --------------
    go_primop op args = primOpIsCheap op && all (exprIsCheap' good_app) args
        -- In principle we should worry about primops
        -- that return a type variable, since the result
        -- might be applied to something, but I'm not going
        -- to bother to check the number of args

    --------------
    go_sel [arg] = exprIsCheap' good_app arg    -- I'm experimenting with making record selection
    go_sel _     = False                -- look cheap, so we will substitute it inside a
                                        -- lambda.  Particularly for dictionary field selection.
                -- BUT: Take care with (sel d x)!  The (sel d) might be cheap, but
                --      there's no guarantee that (sel d x) will be too.  Hence (n_val_args == 1)

-------------------------------------
type CheapAppFun = Id -> Int -> Bool  
  -- Is an application of this function to n *value* args 
  -- always cheap, assuming the arguments are cheap?  
  -- Mainly true of partial applications, data constructors,
  -- and of course true if the number of args is zero

isCheapApp :: CheapAppFun
isCheapApp fn n_val_args
  =  isDataConWorkId fn 
  || n_val_args == 0 
  || n_val_args < idArity fn

isExpandableApp :: CheapAppFun
isExpandableApp fn n_val_args
  =  isConLikeId fn
  || n_val_args < idArity fn
  || go n_val_args (idType fn)
  where
  -- See if all the arguments are PredTys (implicit params or classes)
  -- If so we'll regard it as expandable; see Note [Expandable overloadings]
  -- This incidentally picks up the (n_val_args = 0) case
     go 0 _ = True
     go n_val_args ty
       | Just (_, ty) <- splitForAllTy_maybe ty   = go n_val_args ty
       | Just (arg, ty) <- splitFunTy_maybe ty
       , isPredTy arg                             = go (n_val_args-1) ty
       | otherwise                                = False
\end{code}

Note [Expandable overloadings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose the user wrote this
   {-# RULE  forall x. foo (negate x) = h x #-}
   f x = ....(foo (negate x))....
He'd expect the rule to fire. But since negate is overloaded, we might
get this:
    f = \d -> let n = negate d in \x -> ...foo (n x)...
So we treat the application of a function (negate in this case) to a
*dictionary* as expandable.  In effect, every function is CONLIKE when
it's applied only to dictionaries.


%************************************************************************
%*                                                                      *
             exprOkForSpeculation
%*                                                                      *
%************************************************************************

\begin{code}
-----------------------------
-- | 'exprOkForSpeculation' returns True of an expression that is:
--
--  * Safe to evaluate even if normal order eval might not
--    evaluate the expression at all, or
--
--  * Safe /not/ to evaluate even if normal order would do so
--
-- It is usually called on arguments of unlifted type, but not always
-- In particular, Simplify.rebuildCase calls it on lifted types
-- when a 'case' is a plain 'seq'. See the example in
-- Note [exprOkForSpeculation: case expressions] below
--
-- Precisely, it returns @True@ iff:
--
--  * The expression guarantees to terminate,
--  * soon,
--  * without raising an exception,
--  * without causing a side effect (e.g. writing a mutable variable)
--
-- Note that if @exprIsHNF e@, then @exprOkForSpecuation e@.
-- As an example of the considerations in this test, consider:
--
-- > let x = case y# +# 1# of { r# -> I# r# }
-- > in E
--
-- being translated to:
--
-- > case y# +# 1# of { r# ->
-- >    let x = I# r#
-- >    in E
-- > }
--
-- We can only do this if the @y + 1@ is ok for speculation: it has no
-- side effects, and can't diverge or raise an exception.
exprOkForSpeculation, exprOkForSideEffects :: Expr b -> Bool
exprOkForSpeculation = expr_ok primOpOkForSpeculation
exprOkForSideEffects = expr_ok primOpOkForSideEffects
  -- Polymorphic in binder type
  -- There is one call at a non-Id binder type, in SetLevels

expr_ok :: (PrimOp -> Bool) -> Expr b -> Bool
expr_ok _ (Lit _)      = True
expr_ok _ (Type _)     = True
expr_ok _ (Coercion _) = True
expr_ok primop_ok (Var v)      = app_ok primop_ok v []
expr_ok primop_ok (Cast e _)   = expr_ok primop_ok e

-- Tick annotations that *tick* cannot be speculated, because these
-- are meant to identify whether or not (and how often) the particular
-- source expression was evaluated at runtime.
expr_ok primop_ok (Tick tickish e)
   | tickishCounts tickish = False
   | otherwise             = expr_ok primop_ok e

expr_ok primop_ok (Case e _ _ alts)
  =  expr_ok primop_ok e  -- Note [exprOkForSpeculation: case expressions]
  && all (\(_,_,rhs) -> expr_ok primop_ok rhs) alts
  && altsAreExhaustive alts     -- Note [Exhaustive alts]

expr_ok primop_ok other_expr
  = case collectArgs other_expr of
        (Var f, args) -> app_ok primop_ok f args
        _             -> False

-----------------------------
app_ok :: (PrimOp -> Bool) -> Id -> [Expr b] -> Bool
app_ok primop_ok fun args
  = case idDetails fun of
      DFunId _ new_type ->  not new_type
         -- DFuns terminate, unless the dict is implemented 
         -- with a newtype in which case they may not

      DataConWorkId {} -> True
                -- The strictness of the constructor has already
                -- been expressed by its "wrapper", so we don't need
                -- to take the arguments into account

      PrimOpId op
        | isDivOp op              -- Special case for dividing operations that fail
        , [arg1, Lit lit] <- args -- only if the divisor is zero
        -> not (isZeroLit lit) && expr_ok primop_ok arg1
                  -- Often there is a literal divisor, and this
                  -- can get rid of a thunk in an inner looop

        | DataToTagOp <- op      -- See Note [dataToTag speculation]
        -> True

        | otherwise
        -> primop_ok op        -- A bit conservative: we don't really need
        && all (expr_ok primop_ok) args
                                  
                                  -- to care about lazy arguments, but this is easy

      _other -> isUnLiftedType (idType fun)          -- c.f. the Var case of exprIsHNF
             || idArity fun > n_val_args             -- Partial apps
             || (n_val_args == 0 && 
                 isEvaldUnfolding (idUnfolding fun)) -- Let-bound values
             where
               n_val_args = valArgCount args

-----------------------------
altsAreExhaustive :: [Alt b] -> Bool
-- True  <=> the case alterantives are definiely exhaustive
-- False <=> they may or may not be
altsAreExhaustive []
  = False    -- Should not happen
altsAreExhaustive ((con1,_,_) : alts)
  = case con1 of
      DEFAULT   -> True
      LitAlt {} -> False
      DataAlt c -> 1 + length alts == tyConFamilySize (dataConTyCon c)
      -- It is possible to have an exhaustive case that does not
      -- enumerate all constructors, notably in a GADT match, but
      -- we behave conservatively here -- I don't think it's important
      -- enough to deserve special treatment

-- | True of dyadic operators that can fail only if the second arg is zero!
isDivOp :: PrimOp -> Bool
-- This function probably belongs in PrimOp, or even in
-- an automagically generated file.. but it's such a
-- special case I thought I'd leave it here for now.
isDivOp IntQuotOp        = True
isDivOp IntRemOp         = True
isDivOp WordQuotOp       = True
isDivOp WordRemOp        = True
isDivOp FloatDivOp       = True
isDivOp DoubleDivOp      = True
isDivOp _                = False
\end{code}

Note [exprOkForSpeculation: case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's always sound for exprOkForSpeculation to return False, and we
don't want it to take too long, so it bales out on complicated-looking
terms.  Notably lets, which can be stacked very deeply; and in any
case the argument of exprOkForSpeculation is usually in a strict context,
so any lets will have been floated away.

However, we keep going on case-expressions.  An example like this one
showed up in DPH code (Trac #3717):
    foo :: Int -> Int
    foo 0 = 0
    foo n = (if n < 5 then 1 else 2) `seq` foo (n-1)

If exprOkForSpeculation doesn't look through case expressions, you get this:
    T.$wfoo =
      \ (ww :: GHC.Prim.Int#) ->
        case ww of ds {
          __DEFAULT -> case (case <# ds 5 of _ {
                          GHC.Types.False -> lvl1;
                          GHC.Types.True -> lvl})
                       of _ { __DEFAULT ->
                       T.$wfoo (GHC.Prim.-# ds_XkE 1) };
          0 -> 0
        }

The inner case is redundant, and should be nuked.

Note [Exhaustive alts]
~~~~~~~~~~~~~~~~~~~~~~
We might have something like
  case x of {
    A -> ...
    _ -> ...(case x of { B -> ...; C -> ... })...
Here, the inner case is fine, because the A alternative
can't happen, but it's not ok to float the inner case outside
the outer one (even if we know x is evaluated outside), because
then it would be non-exhaustive. See Trac #5453.

Similarly, this is a valid program (albeit a slightly dodgy one)
   let v = case x of { B -> ...; C -> ... }
   in case x of
         A -> ...
         _ ->  ...v...v....
But we don't want to speculate the v binding.

One could try to be clever, but the easy fix is simpy to regard
a non-exhaustive case as *not* okForSpeculation.


Note [dataToTag speculation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is this OK?
   f x = let v::Int# = dataToTag# x
         in ...
We say "yes", even though 'x' may not be evaluated.  Reasons

  * dataToTag#'s strictness means that its argument often will be
    evaluated, but FloatOut makes that temporarily untrue
         case x of y -> let v = dataToTag# y in ...
    -->
         case x of y -> let v = dataToTag# x in ...
    Note that we look at 'x' instead of 'y' (this is to improve
    floating in FloatOut).  So Lint complains.

    Moreover, it really *might* improve floating to let the
    v-binding float out

  * CorePrep makes sure dataToTag#'s argument is evaluated, just
    before code gen.  Until then, it's not guaranteed


%************************************************************************
%*                                                                      *
             exprIsHNF, exprIsConLike
%*                                                                      *
%************************************************************************

\begin{code}
-- Note [exprIsHNF]             See also Note [exprIsCheap and exprIsHNF]
-- ~~~~~~~~~~~~~~~~
-- | exprIsHNF returns true for expressions that are certainly /already/
-- evaluated to /head/ normal form.  This is used to decide whether it's ok
-- to change:
--
-- > case x of _ -> e
--
--    into:
--
-- > e
--
-- and to decide whether it's safe to discard a 'seq'.
--
-- So, it does /not/ treat variables as evaluated, unless they say they are.
-- However, it /does/ treat partial applications and constructor applications
-- as values, even if their arguments are non-trivial, provided the argument
-- type is lifted. For example, both of these are values:
--
-- > (:) (f x) (map f xs)
-- > map (...redex...)
--
-- because 'seq' on such things completes immediately.
--
-- For unlifted argument types, we have to be careful:
--
-- > C (f x :: Int#)
--
-- Suppose @f x@ diverges; then @C (f x)@ is not a value. However this can't
-- happen: see "CoreSyn#let_app_invariant". This invariant states that arguments of
-- unboxed type must be ok-for-speculation (or trivial).
exprIsHNF :: CoreExpr -> Bool           -- True => Value-lambda, constructor, PAP
exprIsHNF = exprIsHNFlike isDataConWorkId isEvaldUnfolding
\end{code}

\begin{code}
-- | Similar to 'exprIsHNF' but includes CONLIKE functions as well as
-- data constructors. Conlike arguments are considered interesting by the
-- inliner.
exprIsConLike :: CoreExpr -> Bool       -- True => lambda, conlike, PAP
exprIsConLike = exprIsHNFlike isConLikeId isConLikeUnfolding

-- | Returns true for values or value-like expressions. These are lambdas,
-- constructors / CONLIKE functions (as determined by the function argument)
-- or PAPs.
--
exprIsHNFlike :: (Var -> Bool) -> (Unfolding -> Bool) -> CoreExpr -> Bool
exprIsHNFlike is_con is_con_unf = is_hnf_like
  where
    is_hnf_like (Var v) -- NB: There are no value args at this point
      =  is_con v       -- Catches nullary constructors,
                        --      so that [] and () are values, for example
      || idArity v > 0  -- Catches (e.g.) primops that don't have unfoldings
      || is_con_unf (idUnfolding v)
        -- Check the thing's unfolding; it might be bound to a value
        -- We don't look through loop breakers here, which is a bit conservative
        -- but otherwise I worry that if an Id's unfolding is just itself,
        -- we could get an infinite loop

    is_hnf_like (Lit _)          = True
    is_hnf_like (Type _)         = True       -- Types are honorary Values;
                                              -- we don't mind copying them
    is_hnf_like (Coercion _)     = True       -- Same for coercions
    is_hnf_like (Lam b e)        = isRuntimeVar b || is_hnf_like e
    is_hnf_like (Tick tickish e) = not (tickishCounts tickish)
                                      && is_hnf_like e
                                      -- See Note [exprIsHNF Tick]
    is_hnf_like (Cast e _)           = is_hnf_like e
    is_hnf_like (App e (Type _))     = is_hnf_like e
    is_hnf_like (App e (Coercion _)) = is_hnf_like e
    is_hnf_like (App e a)            = app_is_value e [a]
    is_hnf_like (Let _ e)            = is_hnf_like e  -- Lazy let(rec)s don't affect us
    is_hnf_like _                    = False

    -- There is at least one value argument
    app_is_value :: CoreExpr -> [CoreArg] -> Bool
    app_is_value (Var fun) args
      = idArity fun > valArgCount args    -- Under-applied function
        || is_con fun                     --  or constructor-like
    app_is_value (Tick _ f) as = app_is_value f as
    app_is_value (Cast f _) as = app_is_value f as
    app_is_value (App f a)  as = app_is_value f (a:as)
    app_is_value _          _  = False

{-
Note [exprIsHNF Tick]

We can discard source annotations on HNFs as long as they aren't
tick-like:

  scc c (\x . e)    =>  \x . e
  scc c (C x1..xn)  =>  C x1..xn

So we regard these as HNFs.  Tick annotations that tick are not
regarded as HNF if the expression they surround is HNF, because the
tick is there to tell us that the expression was evaluated, so we
don't want to discard a seq on it.
-}
\end{code}


%************************************************************************
%*                                                                      *
             Instantiating data constructors
%*                                                                      *
%************************************************************************

These InstPat functions go here to avoid circularity between DataCon and Id

\begin{code}
dataConRepInstPat   ::                 [Unique] -> DataCon -> [Type] -> ([TyVar], [Id])
dataConRepFSInstPat :: [FastString] -> [Unique] -> DataCon -> [Type] -> ([TyVar], [Id])

dataConRepInstPat   = dataConInstPat (repeat ((fsLit "ipv")))
dataConRepFSInstPat = dataConInstPat

dataConInstPat :: [FastString]          -- A long enough list of FSs to use for names
               -> [Unique]              -- An equally long list of uniques, at least one for each binder
               -> DataCon
               -> [Type]                -- Types to instantiate the universally quantified tyvars
               -> ([TyVar], [Id])          -- Return instantiated variables
-- dataConInstPat arg_fun fss us con inst_tys returns a triple
-- (ex_tvs, arg_ids),
--
--   ex_tvs are intended to be used as binders for existential type args
--
--   arg_ids are indended to be used as binders for value arguments,
--     and their types have been instantiated with inst_tys and ex_tys
--     The arg_ids include both evidence and
--     programmer-specified arguments (both after rep-ing)
--
-- Example.
--  The following constructor T1
--
--  data T a where
--    T1 :: forall b. Int -> b -> T(a,b)
--    ...
--
--  has representation type
--   forall a. forall a1. forall b. (a ~ (a1,b)) =>
--     Int -> b -> T a
--
--  dataConInstPat fss us T1 (a1',b') will return
--
--  ([a1'', b''], [c :: (a1', b')~(a1'', b''), x :: Int, y :: b''])
--
--  where the double-primed variables are created with the FastStrings and
--  Uniques given as fss and us
dataConInstPat fss uniqs con inst_tys 
  = ASSERT( univ_tvs `equalLength` inst_tys )
    (ex_bndrs, arg_ids)
  where 
    univ_tvs = dataConUnivTyVars con
    ex_tvs   = dataConExTyVars con
    arg_tys  = dataConRepArgTys con

    n_ex = length ex_tvs

      -- split the Uniques and FastStrings
    (ex_uniqs, id_uniqs) = splitAt n_ex uniqs
    (ex_fss,   id_fss)   = splitAt n_ex fss

      -- Make the instantiating substitution for universals
    univ_subst = zipOpenTvSubst univ_tvs inst_tys

      -- Make existential type variables, applyingn and extending the substitution
    (full_subst, ex_bndrs) = mapAccumL mk_ex_var univ_subst 
                                       (zip3 ex_tvs ex_fss ex_uniqs)

    mk_ex_var :: TvSubst -> (TyVar, FastString, Unique) -> (TvSubst, TyVar)
    mk_ex_var subst (tv, fs, uniq) = (Type.extendTvSubst subst tv (mkTyVarTy new_tv)
                                     , new_tv)
      where
        new_tv   = mkTyVar new_name kind
        new_name = mkSysTvName uniq fs
        kind     = Type.substTy subst (tyVarKind tv)

      -- Make value vars, instantiating types
    arg_ids = zipWith3 mk_id_var id_uniqs id_fss arg_tys
    mk_id_var uniq fs ty = mkUserLocal (mkVarOccFS fs) uniq 
                                       (Type.substTy full_subst ty) noSrcSpan
\end{code}

%************************************************************************
%*                                                                      *
         Equality
%*                                                                      *
%************************************************************************

\begin{code}
-- | A cheap equality test which bales out fast!
--      If it returns @True@ the arguments are definitely equal,
--      otherwise, they may or may not be equal.
--
-- See also 'exprIsBig'
cheapEqExpr :: Expr b -> Expr b -> Bool

cheapEqExpr (Var v1)   (Var v2)   = v1==v2
cheapEqExpr (Lit lit1) (Lit lit2) = lit1 == lit2
cheapEqExpr (Type t1) (Type t2) = t1 `eqType` t2
cheapEqExpr (Coercion c1) (Coercion c2) = c1 `coreEqCoercion` c2

cheapEqExpr (App f1 a1) (App f2 a2)
  = f1 `cheapEqExpr` f2 && a1 `cheapEqExpr` a2

cheapEqExpr (Cast e1 t1) (Cast e2 t2)
  = e1 `cheapEqExpr` e2 && t1 `coreEqCoercion` t2

cheapEqExpr _ _ = False
\end{code}

\begin{code}
exprIsBig :: Expr b -> Bool
-- ^ Returns @True@ of expressions that are too big to be compared by 'cheapEqExpr'
exprIsBig (Lit _)      = False
exprIsBig (Var _)      = False
exprIsBig (Type _)    = False
exprIsBig (Coercion _) = False
exprIsBig (Lam _ e)    = exprIsBig e
exprIsBig (App f a)    = exprIsBig f || exprIsBig a
exprIsBig (Cast e _)   = exprIsBig e    -- Hopefully coercions are not too big!
exprIsBig _            = True
\end{code}

\begin{code}
eqExpr :: InScopeSet -> CoreExpr -> CoreExpr -> Bool
-- Compares for equality, modulo alpha
eqExpr in_scope e1 e2
  = eqExprX id_unf (mkRnEnv2 in_scope) e1 e2
  where
    id_unf _ = noUnfolding      -- Don't expand
\end{code}

\begin{code}
eqExprX :: IdUnfoldingFun -> RnEnv2 -> CoreExpr -> CoreExpr -> Bool
-- ^ Compares expressions for equality, modulo alpha.
-- Does /not/ look through newtypes or predicate types
-- Used in rule matching, and also CSE

eqExprX id_unfolding_fun env e1 e2
  = go env e1 e2
  where
    go env (Var v1) (Var v2)
      | rnOccL env v1 == rnOccR env v2
      = True

    -- The next two rules expand non-local variables
    -- C.f. Note [Expanding variables] in Rules.lhs
    -- and  Note [Do not expand locally-bound variables] in Rules.lhs
    go env (Var v1) e2
      | not (locallyBoundL env v1)
      , Just e1' <- expandUnfolding_maybe (id_unfolding_fun (lookupRnInScope env v1))
      = go (nukeRnEnvL env) e1' e2

    go env e1 (Var v2)
      | not (locallyBoundR env v2)
      , Just e2' <- expandUnfolding_maybe (id_unfolding_fun (lookupRnInScope env v2))
      = go (nukeRnEnvR env) e1 e2'

    go _   (Lit lit1)    (Lit lit2)      = lit1 == lit2
    go env (Type t1)    (Type t2)        = eqTypeX env t1 t2
    go env (Coercion co1) (Coercion co2) = coreEqCoercion2 env co1 co2
    go env (Cast e1 co1) (Cast e2 co2) = coreEqCoercion2 env co1 co2 && go env e1 e2
    go env (App f1 a1)   (App f2 a2)   = go env f1 f2 && go env a1 a2
    go env (Tick n1 e1)  (Tick n2 e2)  = go_tickish n1 n2 && go env e1 e2

    go env (Lam b1 e1)  (Lam b2 e2)
      =  eqTypeX env (varType b1) (varType b2)   -- False for Id/TyVar combination
      && go (rnBndr2 env b1 b2) e1 e2

    go env (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      =  go env r1 r2  -- No need to check binder types, since RHSs match
      && go (rnBndr2 env v1 v2) e1 e2

    go env (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = all2 (go env') rs1 rs2 && go env' e1 e2
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env' = rnBndrs2 env bs1 bs2

    go env (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives] in TrieMap
      = null a2 && go env e1 e2 && eqTypeX env t1 t2
      | otherwise
      =  go env e1 e2 && all2 (go_alt (rnBndr2 env b1 b2)) a1 a2

    go _ _ _ = False

    -----------
    go_alt env (c1, bs1, e1) (c2, bs2, e2)
      = c1 == c2 && go (rnBndrs2 env bs1 bs2) e1 e2

    -----------
    go_tickish (Breakpoint lid lids) (Breakpoint rid rids)
      = lid == rid  &&  map (rnOccL env) lids == map (rnOccR env) rids
    go_tickish l r = l == r
\end{code}

Auxiliary functions

\begin{code}
locallyBoundL, locallyBoundR :: RnEnv2 -> Var -> Bool
locallyBoundL rn_env v = inRnEnvL rn_env v
locallyBoundR rn_env v = inRnEnvR rn_env v
\end{code}


%************************************************************************
%*                                                                      *
\subsection{The size of an expression}
%*                                                                      *
%************************************************************************

\begin{code}
data CoreStats = CS { cs_tm :: Int    -- Terms
                    , cs_ty :: Int    -- Types
                    , cs_co :: Int }  -- Coercions


instance Outputable CoreStats where 
 ppr (CS { cs_tm = i1, cs_ty = i2, cs_co = i3 })
   = braces (sep [ptext (sLit "terms:")     <+> intWithCommas i1 <> comma,
                  ptext (sLit "types:")     <+> intWithCommas i2 <> comma,
                  ptext (sLit "coercions:") <+> intWithCommas i3])

plusCS :: CoreStats -> CoreStats -> CoreStats
plusCS (CS { cs_tm = p1, cs_ty = q1, cs_co = r1 })
       (CS { cs_tm = p2, cs_ty = q2, cs_co = r2 })
  = CS { cs_tm = p1+p2, cs_ty = q1+q2, cs_co = r1+r2 }

zeroCS, oneTM :: CoreStats
zeroCS = CS { cs_tm = 0, cs_ty = 0, cs_co = 0 }
oneTM  = zeroCS { cs_tm = 1 }

sumCS :: (a -> CoreStats) -> [a] -> CoreStats
sumCS f = foldr (plusCS . f) zeroCS

coreBindsStats :: [CoreBind] -> CoreStats
coreBindsStats = sumCS bindStats

bindStats :: CoreBind -> CoreStats
bindStats (NonRec v r) = bindingStats v r
bindStats (Rec prs)    = sumCS (\(v,r) -> bindingStats v r) prs

bindingStats :: Var -> CoreExpr -> CoreStats
bindingStats v r = bndrStats v `plusCS` exprStats r

bndrStats :: Var -> CoreStats
bndrStats v = oneTM `plusCS` tyStats (varType v)

exprStats :: CoreExpr -> CoreStats
exprStats (Var {})        = oneTM
exprStats (Lit {})        = oneTM
exprStats (Type t)        = tyStats t
exprStats (Coercion c)    = coStats c
exprStats (App f a)       = exprStats f `plusCS` exprStats a
exprStats (Lam b e)       = bndrStats b `plusCS` exprStats e
exprStats (Let b e)       = bindStats b `plusCS` exprStats e
exprStats (Case e b _ as) = exprStats e `plusCS` bndrStats b `plusCS` sumCS altStats as
exprStats (Cast e co)     = coStats co `plusCS` exprStats e
exprStats (Tick _ e)      = exprStats e

altStats :: CoreAlt -> CoreStats
altStats (_, bs, r) = sumCS bndrStats bs `plusCS` exprStats r

tyStats :: Type -> CoreStats
tyStats ty = zeroCS { cs_ty = typeSize ty }

coStats :: Coercion -> CoreStats
coStats co = zeroCS { cs_co = coercionSize co }
\end{code}


\begin{code}
coreBindsSize :: [CoreBind] -> Int
-- We use coreBindStats for user printout
-- but this one is a quick and dirty basis for
-- the simplifier's tick limit
coreBindsSize bs = foldr ((+) . bindSize) 0 bs

exprSize :: CoreExpr -> Int
-- ^ A measure of the size of the expressions, strictly greater than 0
-- It also forces the expression pretty drastically as a side effect
-- Counts *leaves*, not internal nodes. Types and coercions are not counted.
exprSize (Var v)         = v `seq` 1
exprSize (Lit lit)       = lit `seq` 1
exprSize (App f a)       = exprSize f + exprSize a
exprSize (Lam b e)       = bndrSize b + exprSize e
exprSize (Let b e)       = bindSize b + exprSize e
exprSize (Case e b t as) = seqType t `seq` exprSize e + bndrSize b + 1 + foldr ((+) . altSize) 0 as
exprSize (Cast e co)     = (seqCo co `seq` 1) + exprSize e
exprSize (Tick n e)      = tickSize n + exprSize e
exprSize (Type t)        = seqType t `seq` 1
exprSize (Coercion co)   = seqCo co `seq` 1

tickSize :: Tickish Id -> Int
tickSize (ProfNote cc _ _) = cc `seq` 1
tickSize _ = 1 -- the rest are strict

bndrSize :: Var -> Int
bndrSize b | isTyVar b = seqType (tyVarKind b) `seq` 1
           | otherwise = seqType (idType b)             `seq`
                         megaSeqIdInfo (idInfo b)       `seq`
                         1

bndrsSize :: [Var] -> Int
bndrsSize = sum . map bndrSize

bindSize :: CoreBind -> Int
bindSize (NonRec b e) = bndrSize b + exprSize e
bindSize (Rec prs)    = foldr ((+) . pairSize) 0 prs

pairSize :: (Var, CoreExpr) -> Int
pairSize (b,e) = bndrSize b + exprSize e

altSize :: CoreAlt -> Int
altSize (c,bs,e) = c `seq` bndrsSize bs + exprSize e
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Hashing}
%*                                                                      *
%************************************************************************

\begin{code}
hashExpr :: CoreExpr -> Int
-- ^ Two expressions that hash to the same @Int@ may be equal (but may not be)
-- Two expressions that hash to the different Ints are definitely unequal.
--
-- The emphasis is on a crude, fast hash, rather than on high precision.
--
-- But unequal here means \"not identical\"; two alpha-equivalent
-- expressions may hash to the different Ints.
--
-- We must be careful that @\\x.x@ and @\\y.y@ map to the same hash code,
-- (at least if we want the above invariant to be true).

hashExpr e = fromIntegral (hash_expr (1,emptyVarEnv) e .&. 0x7fffffff)
             -- UniqFM doesn't like negative Ints

type HashEnv = (Int, VarEnv Int)  -- Hash code for bound variables

hash_expr :: HashEnv -> CoreExpr -> Word32
-- Word32, because we're expecting overflows here, and overflowing
-- signed types just isn't cool.  In C it's even undefined.
hash_expr env (Tick _ e)              = hash_expr env e
hash_expr env (Cast e _)              = hash_expr env e
hash_expr env (Var v)                 = hashVar env v
hash_expr _   (Lit lit)               = fromIntegral (hashLiteral lit)
hash_expr env (App f e)               = hash_expr env f * fast_hash_expr env e
hash_expr env (Let (NonRec b r) e)    = hash_expr (extend_env env b) e * fast_hash_expr env r
hash_expr env (Let (Rec ((b,_):_)) e) = hash_expr (extend_env env b) e
hash_expr _   (Let (Rec []) _)        = panic "hash_expr: Let (Rec []) _"
hash_expr env (Case e _ _ _)          = hash_expr env e
hash_expr env (Lam b e)               = hash_expr (extend_env env b) e
hash_expr env (Coercion co)           = fast_hash_co env co
hash_expr _   (Type _)                = WARN(True, text "hash_expr: type") 1
-- Shouldn't happen.  Better to use WARN than trace, because trace
-- prevents the CPR optimisation kicking in for hash_expr.

fast_hash_expr :: HashEnv -> CoreExpr -> Word32
fast_hash_expr env (Var v)       = hashVar env v
fast_hash_expr env (Type t)      = fast_hash_type env t
fast_hash_expr env (Coercion co) = fast_hash_co env co
fast_hash_expr _   (Lit lit)     = fromIntegral (hashLiteral lit)
fast_hash_expr env (Cast e _)    = fast_hash_expr env e
fast_hash_expr env (Tick _ e)    = fast_hash_expr env e
fast_hash_expr env (App _ a)     = fast_hash_expr env a -- A bit idiosyncratic ('a' not 'f')!
fast_hash_expr _   _             = 1

fast_hash_type :: HashEnv -> Type -> Word32
fast_hash_type env ty
  | Just tv <- getTyVar_maybe ty            = hashVar env tv
  | Just (tc,tys) <- splitTyConApp_maybe ty = let hash_tc = fromIntegral (hashName (tyConName tc))
                                              in foldr (\t n -> fast_hash_type env t + n) hash_tc tys
  | otherwise                               = 1

fast_hash_co :: HashEnv -> Coercion -> Word32
fast_hash_co env co
  | Just cv <- getCoVar_maybe co              = hashVar env cv
  | Just (tc,cos) <- splitTyConAppCo_maybe co = let hash_tc = fromIntegral (hashName (tyConName tc))
                                                in foldr (\c n -> fast_hash_co env c + n) hash_tc cos
  | otherwise                                 = 1

extend_env :: HashEnv -> Var -> (Int, VarEnv Int)
extend_env (n,env) b = (n+1, extendVarEnv env b n)

hashVar :: HashEnv -> Var -> Word32
hashVar (_,env) v
 = fromIntegral (lookupVarEnv env v `orElse` hashName (idName v))
\end{code}


%************************************************************************
%*                                                                      *
                Eta reduction
%*                                                                      *
%************************************************************************

Note [Eta reduction conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We try for eta reduction here, but *only* if we get all the way to an
trivial expression.  We don't want to remove extra lambdas unless we
are going to avoid allocating this thing altogether.

There are some particularly delicate points here:

* Eta reduction is not valid in general:
        \x. bot  /=  bot
  This matters, partly for old-fashioned correctness reasons but,
  worse, getting it wrong can yield a seg fault. Consider
        f = \x.f x
        h y = case (case y of { True -> f `seq` True; False -> False }) of
                True -> ...; False -> ...

  If we (unsoundly) eta-reduce f to get f=f, the strictness analyser
  says f=bottom, and replaces the (f `seq` True) with just
  (f `cast` unsafe-co).  BUT, as thing stand, 'f' got arity 1, and it
  *keeps* arity 1 (perhaps also wrongly).  So CorePrep eta-expands
  the definition again, so that it does not termninate after all.
  Result: seg-fault because the boolean case actually gets a function value.
  See Trac #1947.

  So it's important to to the right thing.

* Note [Arity care]: we need to be careful if we just look at f's
  arity. Currently (Dec07), f's arity is visible in its own RHS (see
  Note [Arity robustness] in SimplEnv) so we must *not* trust the
  arity when checking that 'f' is a value.  Otherwise we will
  eta-reduce
      f = \x. f x
  to
      f = f
  Which might change a terminating program (think (f `seq` e)) to a
  non-terminating one.  So we check for being a loop breaker first.

  However for GlobalIds we can look at the arity; and for primops we
  must, since they have no unfolding.

* Regardless of whether 'f' is a value, we always want to
  reduce (/\a -> f a) to f
  This came up in a RULE: foldr (build (/\a -> g a))
  did not match           foldr (build (/\b -> ...something complex...))
  The type checker can insert these eta-expanded versions,
  with both type and dictionary lambdas; hence the slightly
  ad-hoc isDictId

* Never *reduce* arity. For example
      f = \xy. g x y
  Then if h has arity 1 we don't want to eta-reduce because then
  f's arity would decrease, and that is bad

These delicacies are why we don't use exprIsTrivial and exprIsHNF here.
Alas.

Note [Eta reduction with casted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    (\(x:t3). f (x |> g)) :: t3 -> t2
  where
    f :: t1 -> t2
    g :: t3 ~ t1
This should be eta-reduced to

    f |> (sym g -> t2)

So we need to accumulate a coercion, pushing it inward (past
variable arguments only) thus:
   f (x |> co_arg) |> co  -->  (f |> (sym co_arg -> co)) x
   f (x:t)         |> co  -->  (f |> (t -> co)) x
   f @ a           |> co  -->  (f |> (forall a.co)) @ a
   f @ (g:t1~t2)   |> co  -->  (f |> (t1~t2 => co)) @ (g:t1~t2)
These are the equations for ok_arg.

It's true that we could also hope to eta reduce these:
    (\xy. (f x |> g) y)
    (\xy. (f x y) |> g)
But the simplifier pushes those casts outwards, so we don't
need to address that here.

\begin{code}
tryEtaReduce :: [Var] -> CoreExpr -> Maybe CoreExpr
tryEtaReduce bndrs body
  = go (reverse bndrs) body (mkReflCo (exprType body))
  where
    incoming_arity = count isId bndrs

    go :: [Var]            -- Binders, innermost first, types [a3,a2,a1]
       -> CoreExpr         -- Of type tr
       -> Coercion         -- Of type tr ~ ts
       -> Maybe CoreExpr   -- Of type a1 -> a2 -> a3 -> ts
    -- See Note [Eta reduction with casted arguments]
    -- for why we have an accumulating coercion
    go [] fun co
      | ok_fun fun = Just (mkCast fun co)

    go (b : bs) (App fun arg) co
      | Just co' <- ok_arg b arg co
      = go bs fun co'

    go _ _ _  = Nothing         -- Failure!

    ---------------
    -- Note [Eta reduction conditions]
    ok_fun (App fun (Type ty))
        | not (any (`elemVarSet` tyVarsOfType ty) bndrs)
        =  ok_fun fun
    ok_fun (Var fun_id)
        =  not (fun_id `elem` bndrs)
        && (ok_fun_id fun_id || all ok_lam bndrs)
    ok_fun _fun = False

    ---------------
    ok_fun_id fun = fun_arity fun >= incoming_arity

    ---------------
    fun_arity fun             -- See Note [Arity care]
       | isLocalId fun
       , isStrongLoopBreaker (idOccInfo fun) = 0
       | arity > 0                           = arity
       | isEvaldUnfolding (idUnfolding fun)  = 1  
            -- See Note [Eta reduction of an eval'd function]
       | otherwise                           = 0
       where
         arity = idArity fun

    ---------------
    ok_lam v = isTyVar v || isEvVar v

    ---------------
    ok_arg :: Var              -- Of type bndr_t
           -> CoreExpr         -- Of type arg_t
           -> Coercion         -- Of kind (t1~t2)
           -> Maybe Coercion   -- Of type (arg_t -> t1 ~  bndr_t -> t2)
                               --   (and similarly for tyvars, coercion args)
    -- See Note [Eta reduction with casted arguments]
    ok_arg bndr (Type ty) co
       | Just tv <- getTyVar_maybe ty
       , bndr == tv  = Just (mkForAllCo tv co)
    ok_arg bndr (Var v) co
       | bndr == v   = Just (mkFunCo (mkReflCo (idType bndr)) co)
    ok_arg bndr (Cast (Var v) co_arg) co
       | bndr == v  = Just (mkFunCo (mkSymCo co_arg) co)
       -- The simplifier combines multiple casts into one,
       -- so we can have a simple-minded pattern match here
    ok_arg _ _ _ = Nothing
\end{code}

Note [Eta reduction of an eval'd function]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Haskell is is not true that    f = \x. f x
because f might be bottom, and 'seq' can distinguish them.

But it *is* true that   f = f `seq` \x. f x 
and we'd like to simplify the latter to the former.  This amounts
to the rule that 
  * when there is just *one* value argument,
  * f is not bottom
we can eta-reduce    \x. f x  ===>  f

This turned up in Trac #7542.  


%************************************************************************
%*                                                                      *
\subsection{Determining non-updatable right-hand-sides}
%*                                                                      *
%************************************************************************

Top-level constructor applications can usually be allocated
statically, but they can't if the constructor, or any of the
arguments, come from another DLL (because we can't refer to static
labels in other DLLs).

If this happens we simply make the RHS into an updatable thunk,
and 'execute' it rather than allocating it statically.

\begin{code}
-- | This function is called only on *top-level* right-hand sides.
-- Returns @True@ if the RHS can be allocated statically in the output,
-- with no thunks involved at all.
rhsIsStatic :: Platform -> (Name -> Bool) -> CoreExpr -> Bool
-- It's called (i) in TidyPgm.hasCafRefs to decide if the rhs is, or
-- refers to, CAFs; (ii) in CoreToStg to decide whether to put an
-- update flag on it and (iii) in DsExpr to decide how to expand
-- list literals
--
-- The basic idea is that rhsIsStatic returns True only if the RHS is
--      (a) a value lambda
--      (b) a saturated constructor application with static args
--
-- BUT watch out for
--  (i) Any cross-DLL references kill static-ness completely
--      because they must be 'executed' not statically allocated
--      ("DLL" here really only refers to Windows DLLs, on other platforms,
--      this is not necessary)
--
-- (ii) We treat partial applications as redexes, because in fact we
--      make a thunk for them that runs and builds a PAP
--      at run-time.  The only appliations that are treated as
--      static are *saturated* applications of constructors.

-- We used to try to be clever with nested structures like this:
--              ys = (:) w ((:) w [])
-- on the grounds that CorePrep will flatten ANF-ise it later.
-- But supporting this special case made the function much more
-- complicated, because the special case only applies if there are no
-- enclosing type lambdas:
--              ys = /\ a -> Foo (Baz ([] a))
-- Here the nested (Baz []) won't float out to top level in CorePrep.
--
-- But in fact, even without -O, nested structures at top level are
-- flattened by the simplifier, so we don't need to be super-clever here.
--
-- Examples
--
--      f = \x::Int. x+7        TRUE
--      p = (True,False)        TRUE
--
--      d = (fst p, False)      FALSE because there's a redex inside
--                              (this particular one doesn't happen but...)
--
--      h = D# (1.0## /## 2.0##)        FALSE (redex again)
--      n = /\a. Nil a                  TRUE
--
--      t = /\a. (:) (case w a of ...) (Nil a)  FALSE (redex)
--
--
-- This is a bit like CoreUtils.exprIsHNF, with the following differences:
--    a) scc "foo" (\x -> ...) is updatable (so we catch the right SCC)
--
--    b) (C x xs), where C is a contructor is updatable if the application is
--         dynamic
--
--    c) don't look through unfolding of f in (f x).

rhsIsStatic platform is_dynamic_name rhs = is_static False rhs
  where
  is_static :: Bool     -- True <=> in a constructor argument; must be atomic
            -> CoreExpr -> Bool

  is_static False (Lam b e)             = isRuntimeVar b || is_static False e
  is_static in_arg (Tick n e)           = not (tickishIsCode n)
                                            && is_static in_arg e
  is_static in_arg (Cast e _)           = is_static in_arg e
  is_static _      (Coercion {})        = True   -- Behaves just like a literal
  is_static _      (Lit (LitInteger {})) = False
  is_static _      (Lit (MachLabel {})) = False
  is_static _      (Lit _)              = True
        -- A MachLabel (foreign import "&foo") in an argument
        -- prevents a constructor application from being static.  The
        -- reason is that it might give rise to unresolvable symbols
        -- in the object file: under Linux, references to "weak"
        -- symbols from the data segment give rise to "unresolvable
        -- relocation" errors at link time This might be due to a bug
        -- in the linker, but we'll work around it here anyway.
        -- SDM 24/2/2004

  is_static in_arg other_expr = go other_expr 0
   where
    go (Var f) n_val_args
        | (platformOS platform /= OSMinGW32) ||
          not (is_dynamic_name (idName f))
        =  saturated_data_con f n_val_args
        || (in_arg && n_val_args == 0)
                -- A naked un-applied variable is *not* deemed a static RHS
                -- E.g.         f = g
                -- Reason: better to update so that the indirection gets shorted
                --         out, and the true value will be seen
                -- NB: if you change this, you'll break the invariant that THUNK_STATICs
                --     are always updatable.  If you do so, make sure that non-updatable
                --     ones have enough space for their static link field!

    go (App f a) n_val_args
        | isTypeArg a                    = go f n_val_args
        | not in_arg && is_static True a = go f (n_val_args + 1)
        -- The (not in_arg) checks that we aren't in a constructor argument;
        -- if we are, we don't allow (value) applications of any sort
        --
        -- NB. In case you wonder, args are sometimes not atomic.  eg.
        --   x = D# (1.0## /## 2.0##)
        -- can't float because /## can fail.

    go (Tick n f) n_val_args = not (tickishIsCode n) && go f n_val_args
    go (Cast e _) n_val_args = go e n_val_args
    go _          _          = False

    saturated_data_con f n_val_args
        = case isDataConWorkId_maybe f of
            Just dc -> n_val_args == dataConRepArity dc
            Nothing -> False
\end{code}
