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

  * Extend the UnariseEnv   x :-> [x1,x2]

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

      - An unboxed tuple pattern. (note that number of binders in the pattern
        will be the same as number of arguments in the scrutinee) e.g.

          case v of x { (# x1, x2, x3 #) -> ... }

        Scrutinee has to be in form `(# t1, t2, t3 #)` so we just extend the
        environment with

          x :-> [t1,t2,t3]
          x1 :-> [t1], x2 :-> [t2], x3 :-> [t3]

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

  ==> (Unarise)

  case (# 1#, x #) of [x1, x2] {
    _ -> f x1 x2
  }

To eliminate this case expression we need to map x1 to 1# in UnariseEnv:

  x1 :-> [1#], x2 :-> [x]

so that `f x1 x2` becomes `f 1# x`.

Note [Unarisation and arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of unarisation, the arity that will be recorded in the generated info
table for an Id may be larger than the idArity. Instead we record what we call
the RepArity, which is the Arity taking into account any expanded arguments, and
corresponds to the number of (possibly-void) *registers* arguments will arrive
in.
-}

{-# LANGUAGE CPP, TupleSections #-}

module UnariseStg (unarise) where

#include "HsVersions.h"

import BasicTypes
import CoreSyn
import DataCon
import FastString (FastString, mkFastString)
import Id
import Literal (Literal (..))
import MkId (voidPrimId, voidArgId)
import MonadUtils (mapAccumLM)
import Outputable
import RepType
import StgSyn
import Type
import TysPrim (intPrimTyCon, intPrimTy)
import TysWiredIn
import UniqSupply
import Util
import VarEnv

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------

-- | A mapping from binders to the Ids they were expanded/renamed to.
--
--    x :-> Unarise [a,b,c] in rho
--
-- iff  x's RepType is a MultiRep, or equivalently
--      x's type is an unboxed tuple or sum, or a void type
--
--    x :-> Rename x'
--
-- iff x's RepType is UnaryRep, or equivalently
--     x's type is not (unboxed tuple, sum, void)
--
-- So
--    x:-> Unarise [a] in rho
-- means x is represented by singleton tuple.
--
-- INVARIANT: Ids in the range only have "unary" types.
--            (i.e. no unboxed tuples or sums)
--
type UnariseEnv = VarEnv UnariseVal

data UnariseVal
  = Unarise [StgArg] -- Unarise to tuple.
  | Rename   StgArg  -- Renaming. See NOTE [Renaming during unarisation].

instance Outputable UnariseVal where
  ppr (Unarise args) = text "Unarise" <+> ppr args
  ppr (Rename arg)   = text "Rename" <+> ppr arg

--------------------------------------------------------------------------------

type OutStgExpr = StgExpr
type InId       = Id
type InStgAlt   = StgAlt
type InStgArg   = StgArg
type OutStgArg  = StgArg

unarise :: UniqSupply -> [StgBinding] -> [StgBinding]
unarise us binds = initUs_ us (mapM (unariseBinding init_env) binds)
  where
    nullary_tup = dataConWorkId unboxedUnitDataCon
    init_env = unitVarEnv nullary_tup (Rename voidArg)

unariseBinding :: UnariseEnv -> StgBinding -> UniqSM StgBinding
unariseBinding rho (StgNonRec x rhs)
  = StgNonRec x <$> unariseRhs rho rhs
unariseBinding rho (StgRec xrhss)
  = StgRec <$> mapM (\(x, rhs) -> (x,) <$> unariseRhs rho rhs) xrhss

unariseRhs :: UnariseEnv -> StgRhs -> UniqSM StgRhs
unariseRhs rho (StgRhsClosure ccs b_info fvs update_flag args expr)
  = do (rho', args1) <- unariseIdBinders rho args
       expr' <- unariseExpr rho' expr
       let fvs' = [ v | StgVarArg v <- unariseIds rho fvs ]
       return (StgRhsClosure ccs b_info fvs' update_flag args1 expr')

unariseRhs rho (StgRhsCon ccs con args)
  = ASSERT (not (isUnboxedTupleCon con || isUnboxedSumCon con))
    return (StgRhsCon ccs con (unariseArgs rho args))

--------------------------------------------------------------------------------

unariseExpr :: UnariseEnv -> StgExpr -> UniqSM StgExpr

unariseExpr rho e@(StgApp f [])
  = case unariseId rho f of
      Just (Unarise args)
        -> return (mkTuple args)
      Just (Rename (StgVarArg f'))
        -> return (StgApp f' [])
      Just (Rename (StgLitArg f'))
        -> return (StgLit f')
      Just (Rename arg@(StgRubbishArg {}))
        -> pprPanic "unariseExpr - app1" (ppr e $$ ppr arg)
      Nothing
        -> return e

unariseExpr rho e@(StgApp f args)
  = return (StgApp f' (unariseArgs rho args))
  where
    f' = case unariseId rho f of
           Just (Rename (StgVarArg f')) -> f'
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
  , let args' = unariseArgs rho args
  = return (StgConApp dc args' (map stgArgType args'))

unariseExpr rho (StgOpApp op args ty)
  = return (StgOpApp op (unariseArgs rho args) ty)

unariseExpr _ e@StgLam{}
  = pprPanic "unariseExpr: found lambda" (ppr e)

unariseExpr rho (StgCase scrut bndr alt_ty alts)
  -- a tuple/sum binders in the scrutinee can always be eliminated
  | StgApp v [] <- scrut
  , Just (Unarise xs) <- unariseId rho v
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
                       -- bndr will be dead after unarise

unariseExpr rho (StgLet bind e)
  = StgLet <$> unariseBinding rho bind <*> unariseExpr rho e

unariseExpr rho (StgLetNoEscape bind e)
  = StgLetNoEscape <$> unariseBinding rho bind <*> unariseExpr rho e

unariseExpr rho (StgTick tick e)
  = StgTick tick <$> unariseExpr rho e

unariseMulti_maybe :: UnariseEnv -> DataCon -> [InStgArg] -> [Type] -> Maybe [StgArg]
unariseMulti_maybe rho dc args ty_args
  | isUnboxedTupleCon dc
  = Just (unariseArgs rho args)

  | isUnboxedSumCon dc
  , let args1 = ASSERT (isSingleton args) (unariseArgs rho args)
  = Just (mkUbxSum dc ty_args args1)

  | otherwise
  = Nothing

--------------------------------------------------------------------------------

elimCase :: UnariseEnv -> [OutStgArg] -> InId -> AltType -> [InStgAlt] -> UniqSM OutStgExpr

elimCase rho args bndr (MultiValAlt _) [(_, bndrs, rhs)]
  = do let rho1 = extendVarEnv rho bndr (Unarise (filterOutVoids args))
           rho2
             | isUnboxedTupleBndr bndr
             = mapTupleIdBinders bndrs args rho1
             | otherwise
             = ASSERT (isUnboxedSumBndr bndr)
               if null bndrs then rho1
                             else mapSumIdBinders bndrs args rho1

       unariseExpr rho2 rhs

elimCase rho args bndr (MultiValAlt _) alts
  | isUnboxedSumBndr bndr
  = do let (tag_arg : real_args) = args
       tag_bndr <- mkId (mkFastString "tag") tagTy
          -- this won't be used but we need a binder anyway
       let rho1 = extendVarEnv rho bndr (Unarise (filterOutVoids args))
           scrut' = case tag_arg of
                      StgVarArg v     -> StgApp v []
                      StgLitArg l     -> StgLit l
                      StgRubbishArg _ -> pprPanic "unariseExpr" (ppr args)

       alts' <- unariseSumAlts rho1 real_args alts
       return (StgCase scrut' tag_bndr tagAltTy alts')

elimCase _ args bndr alt_ty alts
  = pprPanic "elimCase - unhandled case"
      (ppr args <+> ppr bndr <+> ppr alt_ty $$ ppr alts)

--------------------------------------------------------------------------------

unariseAlts :: UnariseEnv -> AltType -> InId -> [StgAlt] -> UniqSM [StgAlt]
unariseAlts rho (MultiValAlt n) bndr [(DEFAULT, [], e)]
  | isUnboxedTupleBndr bndr
  = do (rho', ys) <- unariseIdBinder rho bndr
       e' <- unariseExpr rho' e
       return [(DataAlt (tupleDataCon Unboxed n), ys, e')]

unariseAlts rho (MultiValAlt n) bndr [(DataAlt _, ys, e)]
  | isUnboxedTupleBndr bndr
  = do (rho', ys') <- unariseIdBinders rho ys
       let rho'' = extendVarEnv rho' bndr (Unarise (map StgVarArg ys'))
       e' <- unariseExpr rho'' e
       return [(DataAlt (tupleDataCon Unboxed n), ys', e')]

unariseAlts _ (MultiValAlt _) bndr alts
  | isUnboxedTupleBndr bndr
  = pprPanic "unariseExpr: strange multi val alts" (ppr alts)

-- In this case we don't need to scrutinize the tag bit
unariseAlts rho (MultiValAlt _) bndr [(DEFAULT, _, rhs)]
  | isUnboxedSumBndr bndr
  = do (rho_sum_bndrs, sum_bndrs) <- unariseIdBinder rho bndr
       rhs' <- unariseExpr rho_sum_bndrs rhs
       return [(DataAlt (tupleDataCon Unboxed (length sum_bndrs)), sum_bndrs, rhs')]

unariseAlts rho (MultiValAlt _) bndr alts
  | isUnboxedSumBndr bndr
  = do (rho_sum_bndrs, scrt_bndrs@(tag_bndr : real_bndrs)) <- unariseIdBinder rho bndr
       alts' <- unariseSumAlts rho_sum_bndrs (map StgVarArg real_bndrs) alts
       let inner_case = StgCase (StgApp tag_bndr []) tag_bndr tagAltTy alts'
       return [ (DataAlt (tupleDataCon Unboxed (length scrt_bndrs)),
                 scrt_bndrs,
                 inner_case) ]

unariseAlts rho _ _ alts
  = mapM (\alt -> unariseAlt rho alt) alts

unariseAlt :: UnariseEnv -> StgAlt -> UniqSM StgAlt
unariseAlt rho (con, xs, e)
  = do (rho', xs') <- unariseIdBinders rho xs
       (con, xs',) <$> unariseExpr rho' e

--------------------------------------------------------------------------------

-- | Make alternatives that match on the tag of a sum
-- (i.e. generate LitAlts for the tag)
unariseSumAlts :: UnariseEnv
               -> [StgArg] -- ^ sum components _excluding_ the tag bit.
               -> [StgAlt] -- ^ original alternative with sum LHS
               -> UniqSM [StgAlt]
unariseSumAlts env args alts
  = do alts' <- mapM (unariseSumAlt env args) alts
       return (mkDefaultLitAlt alts')

unariseSumAlt :: UnariseEnv
              -> [StgArg] -- ^ sum components _excluding_ the tag bit.
              -> StgAlt   -- ^ original alternative with sum LHS
              -> UniqSM StgAlt
unariseSumAlt rho _ (DEFAULT, _, e)
  = ( DEFAULT, [], ) <$> unariseExpr rho e

unariseSumAlt rho args (DataAlt sumCon, bs, e)
  = do let rho' = mapSumIdBinders bs args rho
       e' <- unariseExpr rho' e
       return ( LitAlt (MachInt (fromIntegral (dataConTag sumCon))), [], e' )

unariseSumAlt _ scrt alt
  = pprPanic "unariseSumAlt" (ppr scrt $$ ppr alt)

--------------------------------------------------------------------------------

mapTupleIdBinders
  :: [InId]       -- Un-processed binders of a tuple alternative
  -> [OutStgArg]  -- Arguments that form the tuple (after unarisation)
  -> UnariseEnv
  -> UnariseEnv
mapTupleIdBinders ids args0 rho0
  = let
      nv_args = filterOutVoids args0

      ids_unarised :: [(Id, RepType)]
      ids_unarised = map (\id -> (id, repType (idType id))) ids

      map_ids :: UnariseEnv -> [(Id, RepType)] -> [StgArg] -> UnariseEnv
      map_ids rho [] _  = rho
      map_ids rho ((x, x_rep) : xs) args =
        let
          x_arity = length (repTypeSlots x_rep)
          (x_args, args') =
            ASSERT(args `lengthAtLeast` x_arity)
            splitAt x_arity args

          rho'
            | isMultiRep x_rep
            = extendVarEnv rho x (Unarise x_args)
            | otherwise
            = ASSERT (x_args `lengthIs` 1)
              extendVarEnv rho x (Rename (head x_args))
        in
          map_ids rho' xs args'
    in
      map_ids rho0 ids_unarised nv_args

mapSumIdBinders
  :: [InId]      -- Binder of a sum alternative (remember that sum patterns
                 -- only have one binder, so this list should be a singleton)
  -> [OutStgArg] -- Arguments that form the sum (NOT including the tag).
  -> UnariseEnv
  -> UnariseEnv

mapSumIdBinders [id] args0 rho0
  = let
      nv_args   = filterOutVoids args0
      arg_slots = concatMap (repTypeSlots . repType . stgArgType) nv_args
      id_slots  = mapMaybe typeSlotTy (unariseIdType' id)
      layout1   = layout arg_slots id_slots
    in
      if isMultiValBndr id
        then extendVarEnv rho0 id (Unarise [ nv_args !! i | i <- layout1 ])
        else ASSERT(layout1 `lengthIs` 1)
             extendVarEnv rho0 id (Rename (nv_args !! head layout1))

mapSumIdBinders ids sum_args _
  = pprPanic "mapSumIdBinders" (ppr ids $$ ppr sum_args)

-- | Build a unboxed sum term from arguments of an alternative.
mkUbxSum
  :: DataCon      -- Sum data con
  -> [Type]       -- Type arguments of the sum data con
  -> [OutStgArg]  -- Actual arguments of the alternative
  -> [OutStgArg]  -- Final tuple arguments
mkUbxSum dc ty_args args0
  = let
      nv_args = filterOutVoids args0
      (_ : sum_slots) = ubxSumRepType ty_args
        -- drop tag slot

      tag = dataConTag dc

      layout'  = layout sum_slots (mapMaybe (typeSlotTy . stgArgType) nv_args)
      tag_arg  = StgLitArg (MachInt (fromIntegral tag))
      arg_idxs = IM.fromList (zipEqual "mkUbxSum" layout' nv_args)

      mkTupArgs :: Int -> [SlotTy] -> IM.IntMap StgArg -> [StgArg]
      mkTupArgs _ [] _
        = []
      mkTupArgs arg_idx (slot : slots_left) arg_map
        | Just stg_arg <- IM.lookup arg_idx arg_map
        = stg_arg : mkTupArgs (arg_idx + 1) slots_left arg_map
        | otherwise
        = StgRubbishArg (slotTyToType slot) : mkTupArgs (arg_idx + 1) slots_left arg_map
    in
      tag_arg : mkTupArgs 0 sum_slots arg_idxs

--------------------------------------------------------------------------------

unariseArg :: UnariseEnv -> StgArg -> [StgArg]
unariseArg rho (StgVarArg x) = unariseId' rho x
unariseArg _   arg           = [arg]

unariseArgs :: UnariseEnv -> [StgArg] -> [StgArg]
unariseArgs rho = concatMap (unariseArg rho)

unariseId :: UnariseEnv -> Id -> Maybe UnariseVal
unariseId rho x = lookupVarEnv rho x

unariseId' :: UnariseEnv -> Id -> [StgArg]
unariseId' rho x
  = case unariseId rho x of
      Just (Unarise [])   -> [voidArg]
      Just (Unarise args) -> args
      Just (Rename arg)   -> [arg]
      Nothing             -> [StgVarArg x]

unariseIds :: UnariseEnv -> [Id] -> [StgArg]
unariseIds rho = concatMap (unariseId' rho)

-- | A version of `unariseIdBinder` for a list of Ids. Mappings from original
-- Ids to unarised Ids will be accumulated in the rho.
unariseIdBinders :: UnariseEnv -> [Id] -> UniqSM (UnariseEnv, [Id])
unariseIdBinders rho xs = second concat <$> mapAccumLM unariseIdBinder rho xs

-- | Given an Id with potentially unboxed tuple or sum type, returns its "flat"
-- representation by inventing new Ids for tuple or sum's fields. Example:
--
--   unariseIdBinder (x :: (# String, Bool #))
--   ==>
--   [x_1 :: String, x_2 :: Bool]
--
--   unariseIdBinder (x :: (# String | Bool #))
--   ==>
--   [x_1 :: Int#, x_2 :: Any] -- x_1 is the tag
--
-- Also adds a mapping from the original Id to new Ids in the rho.
--
-- Does not update rho and returns the original Id when the it doesn't need
-- unarisation.
unariseIdBinder :: UnariseEnv -> Id -> UniqSM (UnariseEnv, [Id])
unariseIdBinder rho x
  = case unariseIdType x of
      Nothing  ->
        return (rho, [x])
      Just []  ->
        return (extendVarEnv rho x (Unarise []), [voidArgId])
      Just tys -> do
        xs <- mkIds (mkFastString "us") tys
        return (extendVarEnv rho x (Unarise (map StgVarArg xs)), xs)

unariseIdType :: Id -> Maybe [Type]
unariseIdType x =
  case repType (idType x) of
    UnaryRep _     -> Nothing
    MultiRep slots -> Just (map slotTyToType slots)

unariseIdType' :: Id -> [Type]
unariseIdType' x = fromMaybe [idType x] (unariseIdType x)

mkIds :: FastString -> [UnaryType] -> UniqSM [Id]
mkIds fs tys = mapM (mkId fs) tys

mkId :: FastString -> UnaryType -> UniqSM Id
mkId = mkSysLocalOrCoVarM

--------------------------------------------------------------------------------

isMultiValBndr :: Id -> Bool
isMultiValBndr = isMultiRep . repType . idType

isUnboxedSumBndr :: Id -> Bool
isUnboxedSumBndr = isUnboxedSumType . idType

isUnboxedTupleBndr :: Id -> Bool
isUnboxedTupleBndr = isUnboxedTupleType . idType

filterOutVoids :: [StgArg] -> [StgArg]
filterOutVoids = filterOut (isVoidTy . stgArgType)

mkTuple :: [StgArg] -> StgExpr
mkTuple args  = StgConApp (tupleDataCon Unboxed (length args)) args (map stgArgType args)

tagAltTy :: AltType
tagAltTy = PrimAlt intPrimTyCon

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
