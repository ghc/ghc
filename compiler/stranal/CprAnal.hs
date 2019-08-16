{-# LANGUAGE CPP #-}

module CprAnal ( cprAnalProgram ) where

#include "HsVersions.h"

import GhcPrelude

import WwLib            ( deepSplitProductType_maybe )
import Demand
import Cpr
import CoreSyn
import Outputable
import VarEnv
import BasicTypes
import Data.List
import DataCon
import Id
import CoreUtils        ( exprIsHNF )
import TyCon
import Type
import FamInstEnv
import Util
import Maybes           ( isJust )

--
-- * Analysing programs
--

cprAnalProgram :: FamInstEnvs -> CoreProgram -> CoreProgram
cprAnalProgram fam_envs binds
  = snd $ mapAccumL cprAnalTopBind (emptyAnalEnv fam_envs) binds

-- Analyse a (group of) top-level binding(s)
cprAnalTopBind :: AnalEnv
               -> CoreBind
               -> (AnalEnv, CoreBind)
cprAnalTopBind env (NonRec id rhs)
  = (extendAnalEnv env id' (get_idCprInfo id'), NonRec id' rhs')
  where
    (id', rhs') = cprAnalBind TopLevel env id rhs

cprAnalTopBind env (Rec pairs)
  = (env', Rec pairs')
  where
    (env', pairs') = cprFix TopLevel env pairs

--
-- * Analysing expressions
--

-- | The abstract semantic function ⟦_⟧ : Expr -> Env -> A from
-- "Constructed Product Result Analysis for Haskell"
cprAnal, cprAnal'
  :: AnalEnv
  -> CoreExpr            -- ^ expression to be denoted by a 'CprType'
  -> (CprType, CoreExpr) -- ^ the updated expression and its 'CprType'

cprAnal env e = -- pprTraceWith "cprAnal" (\res -> ppr (fst (res)) $$ ppr e) $
                  cprAnal' env e

cprAnal' _ (Lit lit)     = (topCprType, Lit lit)
cprAnal' _ (Type ty)     = (topCprType, Type ty)      -- Doesn't happen, in fact
cprAnal' _ (Coercion co) = (topCprType, Coercion co)

cprAnal' env (Var var)   = (cprTransform env var, Var var)

cprAnal' env (Cast e co)
  = (cpr_ty, Cast e' co)
  where
    (cpr_ty, e') = cprAnal env e

cprAnal' env (Tick t e)
  = (cpr_ty, Tick t e')
  where
    (cpr_ty, e') = cprAnal env e

cprAnal' env (App fun (Type ty))
  = (fun_ty, App fun' (Type ty))
  where
    (fun_ty, fun') = cprAnal env fun

cprAnal' env (App fun arg)
  = (res_ty, App fun' arg')
  where
    (fun_ty, fun') = cprAnal env fun
    -- In contrast to DmdAnal, there is no useful (non-nested) CPR info to be
    -- had by looking into the CprType of arg.
    (_, arg')      = cprAnal env arg
    res_ty         = applyCprTy fun_ty

cprAnal' env (Lam var body)
  | isTyVar var
  , (body_ty, body') <- cprAnal env body
  = (body_ty, Lam var body')
  | otherwise
  = (lam_ty, Lam var body')
  where
    env'             = extendSigsWithLam env var
    (body_ty, body') = cprAnal env' body
    lam_ty           = abstractCprTy body_ty

cprAnal' env (Case scrut case_bndr ty alts)
  = (res_ty, Case scrut' case_bndr ty alts')
  where
    (_, scrut')      = cprAnal env scrut
    -- Regardless whether scrut had the CPR property or not, the case binder
    -- certainly has it. See 'extendEnvForDataAlt'.
    (alt_tys, alts') = mapAndUnzip (cprAnalAlt env scrut case_bndr) alts
    res_ty           = foldl' lubCprType botCprType alt_tys

cprAnal' env (Let (NonRec id rhs) body)
  = (body_ty, Let (NonRec id' rhs') body')
  where
    (id', rhs')      = cprAnalBind NotTopLevel env id rhs
    env'             = extendAnalEnv env id' (get_idCprInfo id')
    (body_ty, body') = cprAnal env' body

cprAnal' env (Let (Rec pairs) body)
  = body_ty `seq` (body_ty, Let (Rec pairs') body')
  where
    (env', pairs')   = cprFix NotTopLevel env pairs
    (body_ty, body') = cprAnal env' body

cprAnalAlt
  :: AnalEnv
  -> CoreExpr -- ^ scrutinee
  -> Id       -- ^ case binder
  -> Alt Var  -- ^ current alternative
  -> (CprType, Alt Var)
cprAnalAlt env scrut case_bndr (con@(DataAlt dc),bndrs,rhs)
  -- See 'extendEnvForDataAlt' and Note [CPR in a DataAlt case alternative]
  = (rhs_ty, (con, bndrs, rhs'))
  where
    env_alt        = extendEnvForDataAlt env scrut case_bndr dc bndrs
    (rhs_ty, rhs') = cprAnal env_alt rhs
cprAnalAlt env _ _ (con,bndrs,rhs)
  = (rhs_ty, (con, bndrs, rhs'))
  where
    (rhs_ty, rhs') = cprAnal env rhs

--
-- * CPR transformer
--

cprTransform :: AnalEnv         -- ^ The analysis environment
             -> Id              -- ^ The function
             -> CprType         -- ^ The demand type of the function
cprTransform env id
  = -- pprTrace "cprTransform" (vcat [ppr id, ppr sig])
    sig
  where
    sig
      | isGlobalId id                   -- imported function or data con worker
      = get_idCprInfo id
      | Just sig <- lookupSigEnv env id -- local let-bound
      = sig
      | otherwise
      = topCprType

--
-- * Bindings
--

-- Recursive bindings
cprFix :: TopLevelFlag
       -> AnalEnv                            -- Does not include bindings for this binding
       -> [(Id,CoreExpr)]
       -> (AnalEnv, [(Id,CoreExpr)]) -- Binders annotated with stricness info

cprFix top_lvl env orig_pairs
  = loop 1 initial_pairs
  where
    -- See Note [Initialising strictness] in DmdAnal.hs
    initial_pairs | ae_virgin env = [(setIdCprInfo id botCpr, rhs) | (id, rhs) <- orig_pairs ]
                  | otherwise     = orig_pairs

    -- The fixed-point varies the idCprInfo field of the binders, and terminates if that
    -- annotation does not change any more.
    loop :: Int -> [(Id,CoreExpr)] -> (AnalEnv, [(Id,CoreExpr)])
    loop n pairs
      | found_fixpoint = (final_anal_env, pairs')
      | otherwise      = loop (n+1) pairs'
      where
        found_fixpoint    = map (idCprInfo . fst) pairs' == map (idCprInfo . fst) pairs
        first_round       = n == 1
        pairs'            = step first_round pairs
        final_anal_env    = extendAnalEnvs env (map fst pairs')

    step :: Bool -> [(Id, CoreExpr)] -> [(Id, CoreExpr)]
    step first_round pairs = pairs'
      where
        -- In all but the first iteration, delete the virgin flag
        start_env | first_round = env
                  | otherwise   = nonVirgin env

        start = extendAnalEnvs start_env (map fst pairs)

        (_, pairs') = mapAccumL my_downRhs start pairs

        my_downRhs env (id,rhs)
          = (env', (id', rhs'))
          where
            (id', rhs') = cprAnalBind top_lvl env id rhs
            env'        = extendAnalEnv env id (get_idCprInfo id')

-- | Process the RHS of the binding for a sensible arity, add the CPR signature
-- to the Id, and augment the environment with the signature as well.
cprAnalBind
  :: TopLevelFlag
  -> AnalEnv
  -> Id
  -> CoreExpr
  -> (Id, CoreExpr)
cprAnalBind top_lvl env id rhs
  = (id', rhs')
  where
    (rhs_ty, rhs')  = cprAnal env rhs
    -- ct_arty rhs_ty might be greater than rhs_arty, so we have to trim it
    -- down. The same situation occurs in DmdAnal; see
    -- Note [Understanding DmdType and StrictSig] in Demand
    -- TODO: Is this really the case? Well, we identify Tops of different
    --       arities with topCprTy, which has arity 0. So, rhs_arty may in fact
    --       be *greater* than ct_arty.
    rhs_ty'
      | isJoinId id
      -- We don't WW join points (see Note [Don't CPR join points] in WorkWrap),
      -- but still want to propagate CPR information.
      -- TODO: Do we? Yes, we do. But we may just ignore trimming CPR for join
      -- points... Or at least try doing so. Let's see what happens.
      = rhs_ty
      | otherwise
      = ensureCprTyArity (idArity id) rhs_ty
    -- possibly trim thunk CPR info
    sig_ty          = trimCprTy trim_all trim_sums rhs_ty'
    id'             = set_idCprInfo id sig_ty

    -- See Note [CPR for thunks]
    -- See Note [CPR for sum types]
    trim_all  = is_thunk && not_strict
    trim_sums = not (isTopLevel top_lvl)
    is_thunk = not (exprIsHNF rhs) && not (isJoinId id)
    not_strict = not (isStrictDmd (idDemandInfo id))

data AnalEnv
  = AE
  { ae_sigs   :: SigEnv
  -- ^ Current approximation of signatures for local ids
  , ae_virgin :: Bool
  -- ^ True only on every first iteration in a fixed-point
  -- iteration. See Note [Initialising strictness] in "DmdAnal"
  , ae_fam_envs :: FamInstEnvs
  -- ^ Needed when expanding type families and synonyms of product types.
  }

type SigEnv = VarEnv CprType

instance Outputable AnalEnv where
  ppr (AE { ae_sigs = env, ae_virgin = virgin })
    = text "AE" <+> braces (vcat
         [ text "ae_virgin =" <+> ppr virgin
         , text "ae_sigs =" <+> ppr env ])

emptyAnalEnv :: FamInstEnvs -> AnalEnv
emptyAnalEnv fam_envs
  = AE
  { ae_sigs = emptyVarEnv
  , ae_virgin = True
  , ae_fam_envs = fam_envs
  }

-- | Extend an environment with the strictness IDs attached to the id
extendAnalEnvs :: AnalEnv -> [Id] -> AnalEnv
extendAnalEnvs env ids
  = env { ae_sigs = sigs' }
  where
    sigs' = extendVarEnvList (ae_sigs env) [ (id, get_idCprInfo id) | id <- ids ]

extendAnalEnv :: AnalEnv -> Id -> CprType -> AnalEnv
extendAnalEnv env id sig
  = env { ae_sigs = extendVarEnv (ae_sigs env) id sig }

lookupSigEnv :: AnalEnv -> Id -> Maybe CprType
lookupSigEnv env id = lookupVarEnv (ae_sigs env) id

nonVirgin :: AnalEnv -> AnalEnv
nonVirgin env = env { ae_virgin = False }

extendSigsWithLam :: AnalEnv -> Id -> AnalEnv
-- Extend the AnalEnv when we meet a lambda binder
extendSigsWithLam env id
  | isId id
  , isStrictDmd (idDemandInfo id) -- See Note [CPR for strict binders]
  , Just (dc,_,_,_) <- deepSplitProductType_maybe (ae_fam_envs env) $ idType id
  = extendAnalEnv env id (prodCprType (dataConRepArity dc))
  | otherwise
  = env

extendEnvForDataAlt :: AnalEnv -> CoreExpr -> Id -> DataCon -> [Var] -> AnalEnv
-- See Note [CPR in a DataAlt case alternative]
extendEnvForDataAlt env scrut case_bndr dc bndrs
  = foldl' do_con_arg env' ids_w_strs
  where
    env' = extendAnalEnv env case_bndr case_bndr_sig

    ids_w_strs    = filter isId bndrs `zip` dataConRepStrictness dc

    tycon          = dataConTyCon dc
    is_product     = isJust (isDataProductTyCon_maybe tycon)
    is_sum         = isJust (isDataSumTyCon_maybe tycon)
    case_bndr_sig
      | is_product = prodCprType (dataConRepArity dc)
      | is_sum     = sumCprType  (dataConTag dc)
      -- Any of the constructors had existentials. This is a little too
      -- conservative (after all, we only care about the particular data con),
      -- but there is no easy way to write is_sum and this won't happen much.
      | otherwise  = topCprType

    -- We could have much deeper CPR info here with Nested CPR, which could
    -- propagate available unboxed things from the scrutinee, getting rid of
    -- the is_var_scrut heuristic. See Note [CPR in a DataAlt case alternative].
    -- Giving strict binders the CPR property only makes sense for products, as
    -- the arguments in Note [CPR for strict binders] don't apply to sums (yet);
    -- we lack WW for strict binders of sum type.
    do_con_arg env (id, str)
       | let is_strict = isStrictDmd (idDemandInfo id) || isMarkedStrict str
       , is_var_scrut && is_strict
       , let fam_envs = ae_fam_envs env
       , Just (dc,_,_,_) <- deepSplitProductType_maybe fam_envs $ idType id
       = extendAnalEnv env id (prodCprType (dataConRepArity dc))
       | otherwise
       = env

    is_var_scrut = is_var scrut
    is_var (Cast e _) = is_var e
    is_var (Var v)    = isLocalId v
    is_var _          = False

set_idCprInfo :: Id -> CprType -> Id
set_idCprInfo id ty = setIdCprInfo id (ct_cpr ty)

get_idCprInfo :: Id -> CprType
get_idCprInfo id = CprType (idArity id) (idCprInfo id)

{- Note [Safe abortion in the fixed-point iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Fixed-point iteration may fail to terminate. But we cannot simply give up and
return the environment and code unchanged! We still need to do one additional
round, to ensure that all expressions have been traversed at least once, and any
unsound CPR annotations have been updated.

Note [CPR in a DataAlt case alternative]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a case alternative, we want to give some of the binders the CPR property.
Specifically

 * The case binder; inside the alternative, the case binder always has
   the CPR property, meaning that a case on it will successfully cancel.
   Example:
        f True  x = case x of y { I# x' -> if x' ==# 3
                                           then y
                                           else I# 8 }
        f False x = I# 3

   By giving 'y' the CPR property, we ensure that 'f' does too, so we get
        f b x = case fw b x of { r -> I# r }
        fw True  x = case x of y { I# x' -> if x' ==# 3 then x' else 8 }
        fw False x = 3

   Of course there is the usual risk of re-boxing: we have 'x' available
   boxed and unboxed, but we return the unboxed version for the wrapper to
   box.  If the wrapper doesn't cancel with its caller, we'll end up
   re-boxing something that we did have available in boxed form.

 * Any strict binders with product type, can use Note [CPR for strict binders].
   But we can go a little further. Consider

      data T = MkT !Int Int

      f2 (MkT x y) | y>0       = f2 (MkT x (y-1))
                   | otherwise = x

   For $wf2 we are going to unbox the MkT *and*, since it is strict, the
   first argument of the MkT; see Note [Add demands for strict constructors].
   But then we don't want box it up again when returning it!  We want
   'f2' to have the CPR property, so we give 'x' the CPR property.

 * It's a bit delicate because if this case is scrutinising something other
   than an argument the original function, we really don't have the unboxed
   version available.  E.g
      g v = case foo v of
              MkT x y | y>0       -> ...
                      | otherwise -> x
   Here we don't have the unboxed 'x' available.  Hence the
   is_var_scrut test when making use of the strictness annotation.
   Slightly ad-hoc, because even if the scrutinee *is* a variable it
   might not be a onre of the arguments to the original function, or a
   sub-component thereof.  But it's simple, and nothing terrible
   happens if we get it wrong.  e.g. Trac #10694.

Note [CPR for strict binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the binder is marked demanded with a strict demand, then give it a
CPR signature. Here's a concrete example ('f1' in test T10482a),
assuming h is strict:

  f1 :: Int -> Int
  f1 x = case h x of
          A -> x
          B -> f1 (x-1)
          C -> x+1

If we notice that 'x' is used strictly, we can give it the CPR
property; and hence f1 gets the CPR property too.  It's sound (doesn't
change strictness) to give it the CPR property because by the time 'x'
is returned (case A above), it'll have been evaluated (by the wrapper
of 'h' in the example).

Moreover, if f itself is strict in x, then we'll pass x unboxed to
f1, and so the boxed version *won't* be available; in that case it's
very helpful to give 'x' the CPR property.

Note that

  * We only want to do this for something that definitely
    has product type, else we may get over-optimistic CPR results
    (e.g. from \x -> x!).

  * See Note [CPR examples]

Note [CPR for sum types]
~~~~~~~~~~~~~~~~~~~~~~~~
At the moment we do not do CPR for let-bindings that
   * non-top level
   * bind a sum type
Reason: I found that in some benchmarks we were losing let-no-escapes,
which messed it all up.  Example
   let j = \x. ....
   in case y of
        True  -> j False
        False -> j True
If we w/w this we get
   let j' = \x. ....
   in case y of
        True  -> case j' False of { (# a #) -> Just a }
        False -> case j' True of { (# a #) -> Just a }
Notice that j' is not a let-no-escape any more.

However this means in turn that the *enclosing* function
may be CPR'd (via the returned Justs).  But in the case of
sums, there may be Nothing alternatives; and that messes
up the sum-type CPR.

Conclusion: only do this for products.  It's still not
guaranteed OK for products, but sums definitely lose sometimes.

Note [CPR for thunks]
~~~~~~~~~~~~~~~~~~~~~
If the rhs is a thunk, we usually forget the CPR info, because
it is presumably shared (else it would have been inlined, and
so we'd lose sharing if w/w'd it into a function).  E.g.

        let r = case expensive of
                  (a,b) -> (b,a)
        in ...

If we marked r as having the CPR property, then we'd w/w into

        let $wr = \() -> case expensive of
                            (a,b) -> (# b, a #)
            r = case $wr () of
                  (# b,a #) -> (b,a)
        in ...

But now r is a thunk, which won't be inlined, so we are no further ahead.
But consider

        f x = let r = case expensive of (a,b) -> (b,a)
              in if foo r then r else (x,x)

Does f have the CPR property?  Well, no.

However, if the strictness analyser has figured out (in a previous
iteration) that it's strict, then we DON'T need to forget the CPR info.
Instead we can retain the CPR info and do the thunk-splitting transform
(see WorkWrap.splitThunk).

This made a big difference to PrelBase.modInt, which had something like
        modInt = \ x -> let r = ... -> I# v in
                        ...body strict in r...
r's RHS isn't a value yet; but modInt returns r in various branches, so
if r doesn't have the CPR property then neither does modInt
Another case I found in practice (in Complex.magnitude), looks like this:
                let k = if ... then I# a else I# b
                in ... body strict in k ....
(For this example, it doesn't matter whether k is returned as part of
the overall result; but it does matter that k's RHS has the CPR property.)
Left to itself, the simplifier will make a join point thus:
                let $j k = ...body strict in k...
                if ... then $j (I# a) else $j (I# b)
With thunk-splitting, we get instead
                let $j x = let k = I#x in ...body strict in k...
                in if ... then $j a else $j b
This is much better; there's a good chance the I# won't get allocated.

But what about botCpr? Consider
    lvl = error "boom"
    fac -1 = lvl
    fac 0 = 1
    fac n = n * fac (n-1)
fac won't have the CPR property here when we trim every thunk! But the
assumption is that error cases are rarely entered and we are diverging anyway,
so WW doesn't hurt.

Note [CPR examples]
~~~~~~~~~~~~~~~~~~~~
Here are some examples (stranal/should_compile/T10482a) of the
usefulness of Note [CPR in a DataAlt case alternative].  The main
point: all of these functions can have the CPR property.

    ------- f1 -----------
    -- x is used strictly by h, so it'll be available
    -- unboxed before it is returned in the True branch

    f1 :: Int -> Int
    f1 x = case h x x of
            True  -> x
            False -> f1 (x-1)


    ------- f2 -----------
    -- x is a strict field of MkT2, so we'll pass it unboxed
    -- to $wf2, so it's available unboxed.  This depends on
    -- the case expression analysing (a subcomponent of) one
    -- of the original arguments to the function, so it's
    -- a bit more delicate.

    data T2 = MkT2 !Int Int

    f2 :: T2 -> Int
    f2 (MkT2 x y) | y>0       = f2 (MkT2 x (y-1))
                  | otherwise = x


    ------- f3 -----------
    -- h is strict in x, so x will be unboxed before it
    -- is rerturned in the otherwise case.

    data T3 = MkT3 Int Int

    f1 :: T3 -> Int
    f1 (MkT3 x y) | h x y     = f3 (MkT3 x (y-1))
                  | otherwise = x


    ------- f4 -----------
    -- Just like f2, but MkT4 can't unbox its strict
    -- argument automatically, as f2 can

    data family Foo a
    newtype instance Foo Int = Foo Int

    data T4 a = MkT4 !(Foo a) Int

    f4 :: T4 Int -> Int
    f4 (MkT4 x@(Foo v) y) | y>0       = f4 (MkT4 x (y-1))
                          | otherwise = v
-}
