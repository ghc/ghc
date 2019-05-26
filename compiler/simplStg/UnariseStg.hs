{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-2012

Note [Unarisation]
~~~~~~~~~~~~~~~~~~
The idea of this pass is to translate away *all* unboxed-tuple and unboxed-sum
binders. So for example:

  f (x :: (# Int, Bool #)) = f x + f (# 1, True #)

  ==>

  f (x1 :: Int) (x2 :: Bool) = f x1 x2 + f 1 True

It is important that we do this at the STG level and NOT at the Core level
because it would be very hard to make this pass Core-type-preserving. In this
example the type of 'f' changes, for example.

STG fed to the code generators *must* be unarised because the code generators do
not support unboxed tuple and unboxed sum binders natively.

In more detail: (see next note for unboxed sums)

Suppose that a variable x : (# t1, t2 #).

  * At the binding site for x, make up fresh vars  x1:t1, x2:t2

  * Extend the UnariseEnv   x :-> MultiVal [x1,x2]

  * Replace the binding with a curried binding for x1,x2

       Lambda:   \x.e                ==>   \x1 x2. e
       Case alt: MkT a b x c d -> e  ==>   MkT a b x1 x2 c d -> e

  * Replace argument occurrences with a sequence of args via a lookup in
    UnariseEnv

       f a b x c d   ==>   f a b x1 x2 c d

  * Replace tail-call occurrences with an unboxed tuple via a lookup in
    UnariseEnv

       x  ==>  (# x1, x2 #)

    So, for example

       f x = x    ==>   f x1 x2 = (# x1, x2 #)

  * We /always/ eliminate a case expression when

       - It scrutinises an unboxed tuple or unboxed sum

       - The scrutinee is a variable (or when it is an explicit tuple, but the
         simplifier eliminates those)

    The case alternative (there can be only one) can be one of these two
    things:

      - An unboxed tuple pattern. e.g.

          case v of x { (# x1, x2, x3 #) -> ... }

        Scrutinee has to be in form `(# t1, t2, t3 #)` so we just extend the
        environment with

          x :-> MultiVal [t1,t2,t3]
          x1 :-> UnaryVal t1, x2 :-> UnaryVal t2, x3 :-> UnaryVal t3

      - A DEFAULT alternative. Just the same, without the bindings for x1,x2,x3

By the end of this pass, we only have unboxed tuples in return positions.
Unboxed sums are completely eliminated, see next note.

Note [Translating unboxed sums to unboxed tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unarise also eliminates unboxed sum binders, and translates unboxed sums in
return positions to unboxed tuples. We want to overlap fields of a sum when
translating it to a tuple to have efficient memory layout. When translating a
sum pattern to a tuple pattern, we need to translate it so that binders of sum
alternatives will be mapped to right arguments after the term translation. So
translation of sum DataCon applications to tuple DataCon applications and
translation of sum patterns to tuple patterns need to be in sync.

These translations work like this. Suppose we have

  (# x1 | | ... #) :: (# t1 | t2 | ... #)

remember that t1, t2 ... can be sums and tuples too. So we first generate
layouts of those. Then we "merge" layouts of each alternative, which gives us a
sum layout with best overlapping possible.

Layout of a flat type 'ty1' is just [ty1].
Layout of a tuple is just concatenation of layouts of its fields.

For layout of a sum type,

  - We first get layouts of all alternatives.
  - We sort these layouts based on their "slot types".
  - We merge all the alternatives.

For example, say we have (# (# Int#, Char #) | (# Int#, Int# #) | Int# #)

  - Layouts of alternatives: [ [Word, Ptr], [Word, Word], [Word] ]
  - Sorted: [ [Ptr, Word], [Word, Word], [Word] ]
  - Merge all alternatives together: [ Ptr, Word, Word ]

We add a slot for the tag to the first position. So our tuple type is

  (# Tag#, Any, Word#, Word# #)
  (we use Any for pointer slots)

Now, any term of this sum type needs to generate a tuple of this type instead.
The translation works by simply putting arguments to first slots that they fit
in. Suppose we had

  (# (# 42#, 'c' #) | | #)

42# fits in Word#, 'c' fits in Any, so we generate this application:

  (# 1#, 'c', 42#, rubbish #)

Another example using the same type: (# | (# 2#, 3# #) | #). 2# fits in Word#,
3# fits in Word #, so we get:

  (# 2#, rubbish, 2#, 3# #).

Note [Types in StgConApp]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have this unboxed sum term:

  (# 123 | #)

What will be the unboxed tuple representation? We can't tell without knowing the
type of this term. For example, these are all valid tuples for this:

  (# 1#, 123 #)          -- when type is (# Int | String #)
  (# 1#, 123, rubbish #) -- when type is (# Int | Float# #)
  (# 1#, 123, rubbish, rubbish #)
                         -- when type is (# Int | (# Int, Int, Int #) #)

So we pass type arguments of the DataCon's TyCon in StgConApp to decide what
layout to use. Note that unlifted values can't be let-bound, so we don't need
types in StgRhsCon.

Note [UnariseEnv can map to literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To avoid redundant case expressions when unarising unboxed sums, UnariseEnv
needs to map variables to literals too. Suppose we have this Core:

  f (# x | #)

  ==> (CorePrep)

  case (# x | #) of y {
    _ -> f y
  }

  ==> (MultiVal)

  case (# 1#, x #) of [x1, x2] {
    _ -> f x1 x2
  }

To eliminate this case expression we need to map x1 to 1# in UnariseEnv:

  x1 :-> UnaryVal 1#, x2 :-> UnaryVal x

so that `f x1 x2` becomes `f 1# x`.

Note [Unarisation and arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of unarisation, the arity that will be recorded in the generated info
table for an Id may be larger than the idArity. Instead we record what we call
the RepArity, which is the Arity taking into account any expanded arguments, and
corresponds to the number of (possibly-void) *registers* arguments will arrive
in.

Note [Post-unarisation invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
STG programs after unarisation have these invariants:

  * No unboxed sums at all.

  * No unboxed tuple binders. Tuples only appear in return position.

  * DataCon applications (StgRhsCon and StgConApp) don't have void arguments.
    This means that it's safe to wrap `StgArg`s of DataCon applications with
    `StgCmmEnv.NonVoid`, for example.

  * Alt binders (binders in patterns) are always non-void.
-}

{-# LANGUAGE CPP, TupleSections #-}

module UnariseStg (unarise) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes
import CoreSyn
import DataCon
import FastString (FastString, mkFastString)
import Id
import Literal
import MkCore (aBSENT_SUM_FIELD_ERROR_ID)
import MkId (voidPrimId, voidArgId)
import MonadUtils (mapAccumLM)
import Outputable
import RepType
import StgSyn
import Type
import TysPrim (intPrimTy,wordPrimTy,word64PrimTy)
import TysWiredIn
import UniqSupply
import Util
import VarEnv

import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------

-- | A mapping from binders to the Ids they were expanded/renamed to.
--
--   x :-> MultiVal [a,b,c] in rho
--
-- iff  x's typePrimRep is not a singleton, or equivalently
--      x's type is an unboxed tuple, sum or void.
--
--    x :-> UnaryVal x'
--
-- iff x's RepType is UnaryRep or equivalently
--     x's type is not unboxed tuple, sum or void.
--
-- So
--     x :-> MultiVal [a] in rho
-- means x is represented by singleton tuple.
--
--     x :-> MultiVal [] in rho
-- means x is void.
--
-- INVARIANT: OutStgArgs in the range only have NvUnaryTypes
--            (i.e. no unboxed tuples, sums or voids)
--
type UnariseEnv = VarEnv UnariseVal

data UnariseVal
  = MultiVal [OutStgArg] -- MultiVal to tuple. Can be empty list (void).
  | UnaryVal OutStgArg   -- See NOTE [Renaming during unarisation].

instance Outputable UnariseVal where
  ppr (MultiVal args) = text "MultiVal" <+> ppr args
  ppr (UnaryVal arg)   = text "UnaryVal" <+> ppr arg

-- | Extend the environment, checking the UnariseEnv invariant.
extendRho :: UnariseEnv -> Id -> UnariseVal -> UnariseEnv
extendRho rho x (MultiVal args)
  = ASSERT(all (isNvUnaryType . stgArgType) args)
    extendVarEnv rho x (MultiVal args)
extendRho rho x (UnaryVal val)
  = ASSERT(isNvUnaryType (stgArgType val))
    extendVarEnv rho x (UnaryVal val)

--------------------------------------------------------------------------------

unarise :: UniqSupply -> [StgTopBinding] -> [StgTopBinding]
unarise us binds = initUs_ us (mapM (unariseTopBinding emptyVarEnv) binds)

unariseTopBinding :: UnariseEnv -> StgTopBinding -> UniqSM StgTopBinding
unariseTopBinding rho (StgTopLifted bind)
  = StgTopLifted <$> unariseBinding rho bind
unariseTopBinding _ bind@StgTopStringLit{} = return bind

unariseBinding :: UnariseEnv -> StgBinding -> UniqSM StgBinding
unariseBinding rho (StgNonRec x rhs)
  = StgNonRec x <$> unariseRhs rho rhs
unariseBinding rho (StgRec xrhss)
  = StgRec <$> mapM (\(x, rhs) -> (x,) <$> unariseRhs rho rhs) xrhss

unariseRhs :: UnariseEnv -> StgRhs -> UniqSM StgRhs
unariseRhs rho (StgRhsClosure ext ccs update_flag args expr)
  = do (rho', args1) <- unariseFunArgBinders rho args
       expr' <- unariseExpr rho' expr
       return (StgRhsClosure ext ccs update_flag args1 expr')

unariseRhs rho (StgRhsCon ccs con args)
  = ASSERT(not (isUnboxedTupleCon con || isUnboxedSumCon con))
    return (StgRhsCon ccs con (unariseConArgs rho args))

--------------------------------------------------------------------------------

unariseExpr :: UnariseEnv -> StgExpr -> UniqSM StgExpr

unariseExpr rho e@(StgApp f [])
  = case lookupVarEnv rho f of
      Just (MultiVal args)  -- Including empty tuples
        -> return (mkTuple args)
      Just (UnaryVal (StgVarArg f'))
        -> return (StgApp f' [])
      Just (UnaryVal (StgLitArg f'))
        -> return (StgLit f')
      Nothing
        -> return e

unariseExpr rho e@(StgApp f args)
  = return (StgApp f' (unariseFunArgs rho args))
  where
    f' = case lookupVarEnv rho f of
           Just (UnaryVal (StgVarArg f')) -> f'
           Nothing -> f
           err -> pprPanic "unariseExpr - app2" (ppr e $$ ppr err)
               -- Can't happen because 'args' is non-empty, and
               -- a tuple or sum cannot be applied to anything

unariseExpr _ (StgLit l)
  = return (StgLit l)

unariseExpr rho (StgConApp dc args ty_args)
  | Just args' <- unariseMulti_maybe rho dc args ty_args
  = return (mkTuple args')

  | otherwise
  , let args' = unariseConArgs rho args
  = return (StgConApp dc args' (map stgArgType args'))

unariseExpr rho (StgOpApp op args ty)
  = return (StgOpApp op (unariseFunArgs rho args) ty)

unariseExpr _ e@StgLam{}
  = pprPanic "unariseExpr: found lambda" (ppr e)

unariseExpr rho (StgCase scrut bndr alt_ty alts)
  -- tuple/sum binders in the scrutinee can always be eliminated
  | StgApp v [] <- scrut
  , Just (MultiVal xs) <- lookupVarEnv rho v
  = elimCase rho xs bndr alt_ty alts

  -- Handle strict lets for tuples and sums:
  --   case (# a,b #) of r -> rhs
  -- and analogously for sums
  | StgConApp dc args ty_args <- scrut
  , Just args' <- unariseMulti_maybe rho dc args ty_args
  = elimCase rho args' bndr alt_ty alts

  -- general case
  | otherwise
  = do scrut' <- unariseExpr rho scrut
       alts'  <- unariseAlts rho alt_ty bndr alts
       return (StgCase scrut' bndr alt_ty alts')
                       -- bndr may have a unboxed sum/tuple type but it will be
                       -- dead after unarise (checked in StgLint)

unariseExpr rho (StgLet ext bind e)
  = StgLet ext <$> unariseBinding rho bind <*> unariseExpr rho e

unariseExpr rho (StgLetNoEscape ext bind e)
  = StgLetNoEscape ext <$> unariseBinding rho bind <*> unariseExpr rho e

unariseExpr rho (StgTick tick e)
  = StgTick tick <$> unariseExpr rho e

-- Doesn't return void args.
unariseMulti_maybe :: UnariseEnv -> DataCon -> [InStgArg] -> [Type] -> Maybe [OutStgArg]
unariseMulti_maybe rho dc args ty_args
  | isUnboxedTupleCon dc
  = Just (unariseConArgs rho args)

  | isUnboxedSumCon dc
  , let args1 = ASSERT(isSingleton args) (unariseConArgs rho args)
  = Just (mkUbxSum dc ty_args args1)

  | otherwise
  = Nothing

--------------------------------------------------------------------------------

elimCase :: UnariseEnv
         -> [OutStgArg] -- non-void args
         -> InId -> AltType -> [InStgAlt] -> UniqSM OutStgExpr

elimCase rho args bndr (MultiValAlt _) [(_, bndrs, rhs)]
  = do let rho1 = extendRho rho bndr (MultiVal args)
           rho2
             | isUnboxedTupleBndr bndr
             = mapTupleIdBinders bndrs args rho1
             | otherwise
             = ASSERT(isUnboxedSumBndr bndr)
               if null bndrs then rho1
                             else mapSumIdBinders bndrs args rho1

       unariseExpr rho2 rhs

elimCase rho args bndr (MultiValAlt _) alts
  | isUnboxedSumBndr bndr
  = do let (tag_arg : real_args) = args
       tag_bndr <- mkId (mkFastString "tag") tagTy
          -- this won't be used but we need a binder anyway
       let rho1 = extendRho rho bndr (MultiVal args)
           scrut' = case tag_arg of
                      StgVarArg v     -> StgApp v []
                      StgLitArg l     -> StgLit l

       alts' <- unariseSumAlts rho1 real_args alts
       return (StgCase scrut' tag_bndr tagAltTy alts')

elimCase _ args bndr alt_ty alts
  = pprPanic "elimCase - unhandled case"
      (ppr args <+> ppr bndr <+> ppr alt_ty $$ ppr alts)

--------------------------------------------------------------------------------

unariseAlts :: UnariseEnv -> AltType -> InId -> [StgAlt] -> UniqSM [StgAlt]
unariseAlts rho (MultiValAlt n) bndr [(DEFAULT, [], e)]
  | isUnboxedTupleBndr bndr
  = do (rho', ys) <- unariseConArgBinder rho bndr
       e' <- unariseExpr rho' e
       return [(DataAlt (tupleDataCon Unboxed n), ys, e')]

unariseAlts rho (MultiValAlt n) bndr [(DataAlt _, ys, e)]
  | isUnboxedTupleBndr bndr
  = do (rho', ys1) <- unariseConArgBinders rho ys
       MASSERT(ys1 `lengthIs` n)
       let rho'' = extendRho rho' bndr (MultiVal (map StgVarArg ys1))
       e' <- unariseExpr rho'' e
       return [(DataAlt (tupleDataCon Unboxed n), ys1, e')]

unariseAlts _ (MultiValAlt _) bndr alts
  | isUnboxedTupleBndr bndr
  = pprPanic "unariseExpr: strange multi val alts" (ppr alts)

-- In this case we don't need to scrutinize the tag bit
unariseAlts rho (MultiValAlt _) bndr [(DEFAULT, _, rhs)]
  | isUnboxedSumBndr bndr
  = do (rho_sum_bndrs, sum_bndrs) <- unariseConArgBinder rho bndr
       rhs' <- unariseExpr rho_sum_bndrs rhs
       return [(DataAlt (tupleDataCon Unboxed (length sum_bndrs)), sum_bndrs, rhs')]

unariseAlts rho (MultiValAlt _) bndr alts
  | isUnboxedSumBndr bndr
  = do (rho_sum_bndrs, scrt_bndrs@(tag_bndr : real_bndrs)) <- unariseConArgBinder rho bndr
       alts' <- unariseSumAlts rho_sum_bndrs (map StgVarArg real_bndrs) alts
       let inner_case = StgCase (StgApp tag_bndr []) tag_bndr tagAltTy alts'
       return [ (DataAlt (tupleDataCon Unboxed (length scrt_bndrs)),
                 scrt_bndrs,
                 inner_case) ]

unariseAlts rho _ _ alts
  = mapM (\alt -> unariseAlt rho alt) alts

unariseAlt :: UnariseEnv -> StgAlt -> UniqSM StgAlt
unariseAlt rho (con, xs, e)
  = do (rho', xs') <- unariseConArgBinders rho xs
       (con, xs',) <$> unariseExpr rho' e

--------------------------------------------------------------------------------

-- | Make alternatives that match on the tag of a sum
-- (i.e. generate LitAlts for the tag)
unariseSumAlts :: UnariseEnv
               -> [StgArg] -- sum components _excluding_ the tag bit.
               -> [StgAlt] -- original alternative with sum LHS
               -> UniqSM [StgAlt]
unariseSumAlts env args alts
  = do alts' <- mapM (unariseSumAlt env args) alts
       return (mkDefaultLitAlt alts')

unariseSumAlt :: UnariseEnv
              -> [StgArg] -- sum components _excluding_ the tag bit.
              -> StgAlt   -- original alternative with sum LHS
              -> UniqSM StgAlt
unariseSumAlt rho _ (DEFAULT, _, e)
  = ( DEFAULT, [], ) <$> unariseExpr rho e

unariseSumAlt rho args (DataAlt sumCon, bs, e)
  = do let rho' = mapSumIdBinders bs args rho
       e' <- unariseExpr rho' e
       return ( LitAlt (LitNumber LitNumInt (fromIntegral (dataConTag sumCon)) intPrimTy), [], e' )

unariseSumAlt _ scrt alt
  = pprPanic "unariseSumAlt" (ppr scrt $$ ppr alt)

--------------------------------------------------------------------------------

mapTupleIdBinders
  :: [InId]       -- Un-processed binders of a tuple alternative.
                  -- Can have void binders.
  -> [OutStgArg]  -- Arguments that form the tuple (after unarisation).
                  -- Can't have void args.
  -> UnariseEnv
  -> UnariseEnv
mapTupleIdBinders ids args0 rho0
  = ASSERT(not (any (isVoidTy . stgArgType) args0))
    let
      ids_unarised :: [(Id, [PrimRep])]
      ids_unarised = map (\id -> (id, typePrimRep (idType id))) ids

      map_ids :: UnariseEnv -> [(Id, [PrimRep])] -> [StgArg] -> UnariseEnv
      map_ids rho [] _  = rho
      map_ids rho ((x, x_reps) : xs) args =
        let
          x_arity = length x_reps
          (x_args, args') =
            ASSERT(args `lengthAtLeast` x_arity)
            splitAt x_arity args

          rho'
            | x_arity == 1
            = ASSERT(x_args `lengthIs` 1)
              extendRho rho x (UnaryVal (head x_args))
            | otherwise
            = extendRho rho x (MultiVal x_args)
        in
          map_ids rho' xs args'
    in
      map_ids rho0 ids_unarised args0

mapSumIdBinders
  :: [InId]      -- Binder of a sum alternative (remember that sum patterns
                 -- only have one binder, so this list should be a singleton)
  -> [OutStgArg] -- Arguments that form the sum (NOT including the tag).
                 -- Can't have void args.
  -> UnariseEnv
  -> UnariseEnv

mapSumIdBinders [id] args rho0
  = ASSERT(not (any (isVoidTy . stgArgType) args))
    let
      arg_slots = map primRepSlot $ concatMap (typePrimRep . stgArgType) args
      id_slots  = map primRepSlot $ typePrimRep (idType id)
      layout1   = layoutUbxSum arg_slots id_slots
    in
      if isMultiValBndr id
        then extendRho rho0 id (MultiVal [ args !! i | i <- layout1 ])
        else ASSERT(layout1 `lengthIs` 1)
             extendRho rho0 id (UnaryVal (args !! head layout1))

mapSumIdBinders ids sum_args _
  = pprPanic "mapSumIdBinders" (ppr ids $$ ppr sum_args)

-- | Build a unboxed sum term from arguments of an alternative.
--
-- Example, for (# x | #) :: (# (# #) | Int #) we call
--
--   mkUbxSum (# _ | #) [ (# #), Int ] [ voidPrimId ]
--
-- which returns
--
--   [ 1#, rubbish ]
--
mkUbxSum
  :: DataCon      -- Sum data con
  -> [Type]       -- Type arguments of the sum data con
  -> [OutStgArg]  -- Actual arguments of the alternative.
  -> [OutStgArg]  -- Final tuple arguments
mkUbxSum dc ty_args args0
  = let
      (_ : sum_slots) = ubxSumRepType (map typePrimRep ty_args)
        -- drop tag slot

      tag = dataConTag dc

      layout'  = layoutUbxSum sum_slots (mapMaybe (typeSlotTy . stgArgType) args0)
      tag_arg  = StgLitArg (LitNumber LitNumInt (fromIntegral tag) intPrimTy)
      arg_idxs = IM.fromList (zipEqual "mkUbxSum" layout' args0)

      mkTupArgs :: Int -> [SlotTy] -> IM.IntMap StgArg -> [StgArg]
      mkTupArgs _ [] _
        = []
      mkTupArgs arg_idx (slot : slots_left) arg_map
        | Just stg_arg <- IM.lookup arg_idx arg_map
        = stg_arg : mkTupArgs (arg_idx + 1) slots_left arg_map
        | otherwise
        = slotRubbishArg slot : mkTupArgs (arg_idx + 1) slots_left arg_map

      slotRubbishArg :: SlotTy -> StgArg
      slotRubbishArg PtrSlot    = StgVarArg aBSENT_SUM_FIELD_ERROR_ID
                         -- See Note [aBSENT_SUM_FIELD_ERROR_ID] in MkCore
      slotRubbishArg WordSlot   = StgLitArg (LitNumber LitNumWord 0 wordPrimTy)
      slotRubbishArg Word64Slot = StgLitArg (LitNumber LitNumWord64 0 word64PrimTy)
      slotRubbishArg FloatSlot  = StgLitArg (LitFloat 0)
      slotRubbishArg DoubleSlot = StgLitArg (LitDouble 0)
    in
      tag_arg : mkTupArgs 0 sum_slots arg_idxs

--------------------------------------------------------------------------------

{-
For arguments (StgArg) and binders (Id) we have two kind of unarisation:

  - When unarising function arg binders and arguments, we don't want to remove
    void binders and arguments. For example,

      f :: (# (# #), (# #) #) -> Void# -> RealWorld# -> ...
      f x y z = <body>

    Here after unarise we should still get a function with arity 3. Similarly
    in the call site we shouldn't remove void arguments:

      f (# (# #), (# #) #) voidId rw

    When unarising <body>, we extend the environment with these binders:

      x :-> MultiVal [], y :-> MultiVal [], z :-> MultiVal []

    Because their rep types are `MultiRep []` (aka. void). This means that when
    we see `x` in a function argument position, we actually replace it with a
    void argument. When we see it in a DataCon argument position, we just get
    rid of it, because DataCon applications in STG are always saturated.

  - When unarising case alternative binders we remove void binders, but we
    still update the environment the same way, because those binders may be
    used in the RHS. Example:

      case x of y {
        (# x1, x2, x3 #) -> <RHS>
      }

    We know that y can't be void, because we don't scrutinize voids, so x will
    be unarised to some number of arguments, and those arguments will have at
    least one non-void thing. So in the rho we will have something like:

      x :-> MultiVal [xu1, xu2]

    Now, after we eliminate void binders in the pattern, we get exactly the same
    number of binders, and extend rho again with these:

      x1 :-> UnaryVal xu1
      x2 :-> MultiVal [] -- x2 is void
      x3 :-> UnaryVal xu2

    Now when we see x2 in a function argument position or in return position, we
    generate void#. In constructor argument position, we just remove it.

So in short, when we have a void id,

  - We keep it if it's a lambda argument binder or
                       in argument position of an application.

  - We remove it if it's a DataCon field binder or
                         in argument position of a DataCon application.
-}

unariseArgBinder
    :: Bool -- data con arg?
    -> UnariseEnv -> Id -> UniqSM (UnariseEnv, [Id])
unariseArgBinder is_con_arg rho x =
  case typePrimRep (idType x) of
    []
      | is_con_arg
      -> return (extendRho rho x (MultiVal []), [])
      | otherwise -- fun arg, do not remove void binders
      -> return (extendRho rho x (MultiVal []), [voidArgId])

    [rep]
      -- Arg represented as single variable, but original type may still be an
      -- unboxed sum/tuple, e.g. (# Void# | Void# #).
      --
      -- While not unarising the binder in this case does not break any programs
      -- (because it unarises to a single variable), it triggers StgLint as we
      -- break the the post-unarisation invariant that says unboxed tuple/sum
      -- binders should vanish. See Note [Post-unarisation invariants].
      | isUnboxedSumType (idType x) || isUnboxedTupleType (idType x)
      -> do x' <- mkId (mkFastString "us") (primRepToType rep)
            return (extendRho rho x (MultiVal [StgVarArg x']), [x'])
      | otherwise
      -> return (rho, [x])

    reps -> do
      xs <- mkIds (mkFastString "us") (map primRepToType reps)
      return (extendRho rho x (MultiVal (map StgVarArg xs)), xs)

--------------------------------------------------------------------------------

-- | MultiVal a function argument. Never returns an empty list.
unariseFunArg :: UnariseEnv -> StgArg -> [StgArg]
unariseFunArg rho (StgVarArg x) =
  case lookupVarEnv rho x of
    Just (MultiVal [])  -> [voidArg]   -- NB: do not remove void args
    Just (MultiVal as)  -> as
    Just (UnaryVal arg) -> [arg]
    Nothing             -> [StgVarArg x]
unariseFunArg _ arg = [arg]

unariseFunArgs :: UnariseEnv -> [StgArg] -> [StgArg]
unariseFunArgs = concatMap . unariseFunArg

unariseFunArgBinders :: UnariseEnv -> [Id] -> UniqSM (UnariseEnv, [Id])
unariseFunArgBinders rho xs = second concat <$> mapAccumLM unariseFunArgBinder rho xs

-- Result list of binders is never empty
unariseFunArgBinder :: UnariseEnv -> Id -> UniqSM (UnariseEnv, [Id])
unariseFunArgBinder = unariseArgBinder False

--------------------------------------------------------------------------------

-- | MultiVal a DataCon argument. Returns an empty list when argument is void.
unariseConArg :: UnariseEnv -> InStgArg -> [OutStgArg]
unariseConArg rho (StgVarArg x) =
  case lookupVarEnv rho x of
    Just (UnaryVal arg) -> [arg]
    Just (MultiVal as) -> as      -- 'as' can be empty
    Nothing
      | isVoidTy (idType x) -> [] -- e.g. C realWorld#
                                  -- Here realWorld# is not in the envt, but
                                  -- is a void, and so should be eliminated
      | otherwise -> [StgVarArg x]
unariseConArg _ arg@(StgLitArg lit) =
    ASSERT(not (isVoidTy (literalType lit)))  -- We have no void literals
    [arg]

unariseConArgs :: UnariseEnv -> [InStgArg] -> [OutStgArg]
unariseConArgs = concatMap . unariseConArg

unariseConArgBinders :: UnariseEnv -> [Id] -> UniqSM (UnariseEnv, [Id])
unariseConArgBinders rho xs = second concat <$> mapAccumLM unariseConArgBinder rho xs

-- Different from `unariseFunArgBinder`: result list of binders may be empty.
-- See DataCon applications case in Note [Post-unarisation invariants].
unariseConArgBinder :: UnariseEnv -> Id -> UniqSM (UnariseEnv, [Id])
unariseConArgBinder = unariseArgBinder True

--------------------------------------------------------------------------------

mkIds :: FastString -> [UnaryType] -> UniqSM [Id]
mkIds fs tys = mapM (mkId fs) tys

mkId :: FastString -> UnaryType -> UniqSM Id
mkId = mkSysLocalOrCoVarM

isMultiValBndr :: Id -> Bool
isMultiValBndr id
  | [_] <- typePrimRep (idType id)
  = False
  | otherwise
  = True

isUnboxedSumBndr :: Id -> Bool
isUnboxedSumBndr = isUnboxedSumType . idType

isUnboxedTupleBndr :: Id -> Bool
isUnboxedTupleBndr = isUnboxedTupleType . idType

mkTuple :: [StgArg] -> StgExpr
mkTuple args = StgConApp (tupleDataCon Unboxed (length args)) args (map stgArgType args)

tagAltTy :: AltType
tagAltTy = PrimAlt IntRep

tagTy :: Type
tagTy = intPrimTy

voidArg :: StgArg
voidArg = StgVarArg voidPrimId

mkDefaultLitAlt :: [StgAlt] -> [StgAlt]
-- We have an exhauseive list of literal alternatives
--    1# -> e1
--    2# -> e2
-- Since they are exhaustive, we can replace one with DEFAULT, to avoid
-- generating a final test. Remember, the DEFAULT comes first if it exists.
mkDefaultLitAlt [] = pprPanic "elimUbxSumExpr.mkDefaultAlt" (text "Empty alts")
mkDefaultLitAlt alts@((DEFAULT, _, _) : _) = alts
mkDefaultLitAlt ((LitAlt{}, [], rhs) : alts) = (DEFAULT, [], rhs) : alts
mkDefaultLitAlt alts = pprPanic "mkDefaultLitAlt" (text "Not a lit alt:" <+> ppr alts)
