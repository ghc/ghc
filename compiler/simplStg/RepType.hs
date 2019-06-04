{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module RepType
  (
    -- * Code generator views onto Types
    UnaryType, NvUnaryType, isNvUnaryType,
    unwrapType,

    -- * Predicates on types
    isVoidTy,

    -- * Type representation for the code generator
    typePrimRep, typePrimRep1,
    runtimeRepPrimRep, typePrimRepArgs,
    PrimRep(..), primRepToType,
    countFunRepArgs, countConRepArgs, tyConPrimRep, tyConPrimRep1,

    -- * Unboxed sum representation type
    ubxSumRepType, layoutUbxSum, typeSlotTy, SlotTy (..),
    slotPrimRep, primRepSlot
  ) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes (Arity, RepArity)
import DataCon
import Outputable
import PrelNames
import Coercion
import TyCon
import TyCoRep
import Type
import Util
import TysPrim
import {-# SOURCE #-} TysWiredIn ( anyTypeOfKind )

import Data.List (sort)
import qualified Data.IntSet as IS

{- **********************************************************************
*                                                                       *
                Representation types
*                                                                       *
********************************************************************** -}

type NvUnaryType = Type
type UnaryType   = Type
     -- Both are always a value type; i.e. its kind is TYPE rr
     -- for some rr; moreover the rr is never a variable.
     --
     --   NvUnaryType : never an unboxed tuple or sum, or void
     --
     --   UnaryType   : never an unboxed tuple or sum;
     --                 can be Void# or (# #)

isNvUnaryType :: Type -> Bool
isNvUnaryType ty
  | [_] <- typePrimRep ty
  = True
  | otherwise
  = False

-- INVARIANT: the result list is never empty.
typePrimRepArgs :: HasDebugCallStack => Type -> [PrimRep]
typePrimRepArgs ty
  | [] <- reps
  = [VoidRep]
  | otherwise
  = reps
  where
    reps = typePrimRep ty

-- | Gets rid of the stuff that prevents us from understanding the
-- runtime representation of a type. Including:
--   1. Casts
--   2. Newtypes
--   3. Foralls
--   4. Synonyms
-- But not type/data families, because we don't have the envs to hand.
unwrapType :: Type -> Type
unwrapType ty
  | Just (_, unwrapped)
      <- topNormaliseTypeX stepper mappend inner_ty
  = unwrapped
  | otherwise
  = inner_ty
  where
    inner_ty = go ty

    go t | Just t' <- coreView t = go t'
    go (ForAllTy _ t)            = go t
    go (CastTy t _)              = go t
    go t                         = t

     -- cf. Coercion.unwrapNewTypeStepper
    stepper rec_nts tc tys
      | Just (ty', _) <- instNewTyCon_maybe tc tys
      = case checkRecTc rec_nts tc of
          Just rec_nts' -> NS_Step rec_nts' (go ty') ()
          Nothing       -> NS_Abort   -- infinite newtypes
      | otherwise
      = NS_Done

countFunRepArgs :: Arity -> Type -> RepArity
countFunRepArgs 0 _
  = 0
countFunRepArgs n ty
  | FunTy arg res <- unwrapType ty
  = length (typePrimRepArgs arg) + countFunRepArgs (n - 1) res
  | otherwise
  = pprPanic "countFunRepArgs: arity greater than type can handle" (ppr (n, ty, typePrimRep ty))

countConRepArgs :: DataCon -> RepArity
countConRepArgs dc = go (dataConRepArity dc) (dataConRepType dc)
  where
    go :: Arity -> Type -> RepArity
    go 0 _
      = 0
    go n ty
      | FunTy arg res <- unwrapType ty
      = length (typePrimRep arg) + go (n - 1) res
      | otherwise
      = pprPanic "countConRepArgs: arity greater than type can handle" (ppr (n, ty, typePrimRep ty))

-- | True if the type has zero width.
isVoidTy :: Type -> Bool
isVoidTy = null . typePrimRep


{- **********************************************************************
*                                                                       *
                Unboxed sums
 See Note [Translating unboxed sums to unboxed tuples] in UnariseStg.hs
*                                                                       *
********************************************************************** -}

type SortedSlotTys = [SlotTy]

-- | Given the arguments of a sum type constructor application,
--   return the unboxed sum rep type.
--
-- E.g.
--
--   (# Int# | Maybe Int | (# Int#, Float# #) #)
--
-- We call `ubxSumRepType [ [IntRep], [LiftedRep], [IntRep, FloatRep] ]`,
-- which returns [WordSlot, PtrSlot, WordSlot, FloatSlot]
--
-- INVARIANT: Result slots are sorted (via Ord SlotTy), except that at the head
-- of the list we have the slot for the tag.
ubxSumRepType :: [[PrimRep]] -> [SlotTy]
ubxSumRepType constrs0
  -- These first two cases never classify an actual unboxed sum, which always
  -- has at least two disjuncts. But it could happen if a user writes, e.g.,
  -- forall (a :: TYPE (SumRep [IntRep])). ...
  -- which could never be instantiated. We still don't want to panic.
  | constrs0 `lengthLessThan` 2
  = [WordSlot]

  | otherwise
  = let
      combine_alts :: [SortedSlotTys]  -- slots of constructors
                   -> SortedSlotTys    -- final slots
      combine_alts constrs = foldl' merge [] constrs

      merge :: SortedSlotTys -> SortedSlotTys -> SortedSlotTys
      merge existing_slots []
        = existing_slots
      merge [] needed_slots
        = needed_slots
      merge (es : ess) (s : ss)
        | Just s' <- s `fitsIn` es
        = -- found a slot, use it
          s' : merge ess ss
        | s < es
        = -- we need a new slot and this is the right place for it
          s : merge (es : ess) ss
        | otherwise
        = -- keep searching for a slot
          es : merge ess (s : ss)

      -- Nesting unboxed tuples and sums is OK, so we need to flatten first.
      rep :: [PrimRep] -> SortedSlotTys
      rep ty = sort (map primRepSlot ty)

      sumRep = WordSlot : combine_alts (map rep constrs0)
               -- WordSlot: for the tag of the sum
    in
      sumRep

layoutUbxSum :: SortedSlotTys -- Layout of sum. Does not include tag.
                              -- We assume that they are in increasing order
             -> [SlotTy]      -- Slot types of things we want to map to locations in the
                              -- sum layout
             -> [Int]         -- Where to map 'things' in the sum layout
layoutUbxSum sum_slots0 arg_slots0 =
    go arg_slots0 IS.empty
  where
    go :: [SlotTy] -> IS.IntSet -> [Int]
    go [] _
      = []
    go (arg : args) used
      = let slot_idx = findSlot arg 0 sum_slots0 used
         in slot_idx : go args (IS.insert slot_idx used)

    findSlot :: SlotTy -> Int -> SortedSlotTys -> IS.IntSet -> Int
    findSlot arg slot_idx (slot : slots) useds
      | not (IS.member slot_idx useds)
      , Just slot == arg `fitsIn` slot
      = slot_idx
      | otherwise
      = findSlot arg (slot_idx + 1) slots useds
    findSlot _ _ [] _
      = pprPanic "findSlot" (text "Can't find slot" $$ ppr sum_slots0 $$ ppr arg_slots0)

--------------------------------------------------------------------------------

-- We have 3 kinds of slots:
--
--   - Pointer slot: Only shared between actual pointers to Haskell heap (i.e.
--     boxed objects)
--
--   - Word slots: Shared between IntRep, WordRep, Int64Rep, Word64Rep, AddrRep.
--
--   - Float slots: Shared between floating point types.
--
--   - Void slots: Shared between void types. Not used in sums.
--
-- TODO(michalt): We should probably introduce `SlotTy`s for 8-/16-/32-bit
-- values, so that we can pack things more tightly.
data SlotTy = PtrSlot | WordSlot | Word64Slot | FloatSlot | DoubleSlot
  deriving (Eq, Ord)
    -- Constructor order is important! If slot A could fit into slot B
    -- then slot A must occur first.  E.g.  FloatSlot before DoubleSlot
    --
    -- We are assuming that WordSlot is smaller than or equal to Word64Slot
    -- (would not be true on a 128-bit machine)

instance Outputable SlotTy where
  ppr PtrSlot    = text "PtrSlot"
  ppr Word64Slot = text "Word64Slot"
  ppr WordSlot   = text "WordSlot"
  ppr DoubleSlot = text "DoubleSlot"
  ppr FloatSlot  = text "FloatSlot"

typeSlotTy :: UnaryType -> Maybe SlotTy
typeSlotTy ty
  | isVoidTy ty
  = Nothing
  | otherwise
  = Just (primRepSlot (typePrimRep1 ty))

primRepSlot :: PrimRep -> SlotTy
primRepSlot VoidRep     = pprPanic "primRepSlot" (text "No slot for VoidRep")
primRepSlot LiftedRep   = PtrSlot
primRepSlot UnliftedRep = PtrSlot
primRepSlot IntRep      = WordSlot
primRepSlot Int8Rep     = WordSlot
primRepSlot Int16Rep    = WordSlot
primRepSlot Int64Rep    = Word64Slot
primRepSlot WordRep     = WordSlot
primRepSlot Word8Rep    = WordSlot
primRepSlot Word16Rep   = WordSlot
primRepSlot Word64Rep   = Word64Slot
primRepSlot AddrRep     = WordSlot
primRepSlot FloatRep    = FloatSlot
primRepSlot DoubleRep   = DoubleSlot
primRepSlot VecRep{}    = pprPanic "primRepSlot" (text "No slot for VecRep")

slotPrimRep :: SlotTy -> PrimRep
slotPrimRep PtrSlot     = LiftedRep   -- choice between lifted & unlifted seems arbitrary
slotPrimRep Word64Slot  = Word64Rep
slotPrimRep WordSlot    = WordRep
slotPrimRep DoubleSlot  = DoubleRep
slotPrimRep FloatSlot   = FloatRep

-- | Returns the bigger type if one fits into the other. (commutative)
fitsIn :: SlotTy -> SlotTy -> Maybe SlotTy
fitsIn ty1 ty2
  | isWordSlot ty1 && isWordSlot ty2
  = Just (max ty1 ty2)
  | isFloatSlot ty1 && isFloatSlot ty2
  = Just (max ty1 ty2)
  | isPtrSlot ty1 && isPtrSlot ty2
  = Just PtrSlot
  | otherwise
  = Nothing
  where
    isPtrSlot PtrSlot = True
    isPtrSlot _       = False

    isWordSlot Word64Slot = True
    isWordSlot WordSlot   = True
    isWordSlot _          = False

    isFloatSlot DoubleSlot = True
    isFloatSlot FloatSlot  = True
    isFloatSlot _          = False


{- **********************************************************************
*                                                                       *
                   PrimRep
*                                                                       *
********************************************************************** -}

-- | Discovers the primitive representation of a 'Type'. Returns
-- a list of 'PrimRep': it's a list because of the possibility of
-- no runtime representation (void) or multiple (unboxed tuple/sum)
typePrimRep :: HasDebugCallStack => Type -> [PrimRep]
typePrimRep ty = kindPrimRep (text "typePrimRep" <+>
                              parens (ppr ty <+> dcolon <+> ppr (typeKind ty)))
                             (typeKind ty)

-- | Like 'typePrimRep', but assumes that there is precisely one 'PrimRep' output;
-- an empty list of PrimReps becomes a VoidRep
typePrimRep1 :: HasDebugCallStack => UnaryType -> PrimRep
typePrimRep1 ty = case typePrimRep ty of
  []    -> VoidRep
  [rep] -> rep
  _     -> pprPanic "typePrimRep1" (ppr ty $$ ppr (typePrimRep ty))

-- | Find the runtime representation of a 'TyCon'. Defined here to
-- avoid module loops. Returns a list of the register shapes necessary.
tyConPrimRep :: HasDebugCallStack => TyCon -> [PrimRep]
tyConPrimRep tc
  = kindPrimRep (text "kindRep tc" <+> ppr tc $$ ppr res_kind)
                res_kind
  where
    res_kind = tyConResKind tc

-- | Like 'tyConPrimRep', but assumed that there is precisely zero or
-- one 'PrimRep' output
tyConPrimRep1 :: HasDebugCallStack => TyCon -> PrimRep
tyConPrimRep1 tc = case tyConPrimRep tc of
  []    -> VoidRep
  [rep] -> rep
  _     -> pprPanic "tyConPrimRep1" (ppr tc $$ ppr (tyConPrimRep tc))

-- | Take a kind (of shape @TYPE rr@) and produce the 'PrimRep's
-- of values of types of this kind.
kindPrimRep :: HasDebugCallStack => SDoc -> Kind -> [PrimRep]
kindPrimRep doc ki
  | Just ki' <- coreView ki
  = kindPrimRep doc ki'
kindPrimRep doc (TyConApp typ [runtime_rep])
  = ASSERT( typ `hasKey` tYPETyConKey )
    runtimeRepPrimRep doc runtime_rep
kindPrimRep doc ki
  = pprPanic "kindPrimRep" (ppr ki $$ doc)

-- | Take a type of kind RuntimeRep and extract the list of 'PrimRep' that
-- it encodes.
runtimeRepPrimRep :: HasDebugCallStack => SDoc -> Type -> [PrimRep]
runtimeRepPrimRep doc rr_ty
  | Just rr_ty' <- coreView rr_ty
  = runtimeRepPrimRep doc rr_ty'
  | TyConApp rr_dc args <- rr_ty
  , RuntimeRep fun <- tyConRuntimeRepInfo rr_dc
  = fun args
  | otherwise
  = pprPanic "runtimeRepPrimRep" (doc $$ ppr rr_ty)

-- | Convert a PrimRep back to a Type. Used only in the unariser to give types
-- to fresh Ids. Really, only the type's representation matters.
primRepToType :: PrimRep -> Type
primRepToType = anyTypeOfKind . tYPE . primRepToRuntimeRep
