{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module RepType
  ( -- * Code generator views onto Types
    UnaryType, NvUnaryType, isNvUnaryType,
    RepType(..), repType, repTypeArgs, isUnaryRep, isMultiRep,

    -- * Predicates on types
    isVoidTy, typePrimRep,

    -- * Type representation for the code generator
    countConRepArgs, idFunRepArity, tyConPrimRep,

    -- * Unboxed sum representation type
    ubxSumRepType, layout, typeSlotTy, SlotTy (..), slotTyToType,
    slotPrimRep, repTypeSlots
  ) where

#include "HsVersions.h"

import BasicTypes (Arity, RepArity)
import DataCon
import Id
import Outputable
import PrelNames
import TyCon
import TyCoRep
import Type
import TysPrim
import TysWiredIn
import Util

import Data.List (foldl', sort)
import Data.Maybe (maybeToList)
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
     --                 can be Void# (but not (# #))

isNvUnaryType :: Type -> Bool
isNvUnaryType ty
  = case repType ty of
      UnaryRep _  -> True
      MultiRep ss -> not (null ss)

data RepType
  = MultiRep [SlotTy]     -- Represented by multiple values (e.g. unboxed tuple or sum)
  | UnaryRep NvUnaryType  -- Represented by a single value; but never Void#, or any
                          -- other zero-width type (isVoidTy)

instance Outputable RepType where
  ppr (MultiRep slots) = text "MultiRep" <+> ppr slots
  ppr (UnaryRep ty)    = text "UnaryRep" <+> ppr ty

isMultiRep :: RepType -> Bool
isMultiRep (MultiRep _) = True
isMultiRep _            = False

isUnaryRep :: RepType -> Bool
isUnaryRep (UnaryRep _) = True
isUnaryRep _            = False

-- INVARIANT: the result list is never empty.
repTypeArgs :: Type -> [UnaryType]
repTypeArgs ty = case repType ty of
                    MultiRep []    -> [voidPrimTy]
                    MultiRep slots -> map slotTyToType slots
                    UnaryRep ty    -> [ty]

repTypeSlots :: RepType -> [SlotTy]
repTypeSlots (MultiRep slots) = slots
repTypeSlots (UnaryRep ty)    = maybeToList (typeSlotTy ty)

-- | 'repType' figure out how a type will be represented at runtime. It looks
-- through
--
--      1. For-alls
--      2. Synonyms
--      3. Predicates
--      4. All newtypes, including recursive ones, but not newtype families
--      5. Casts
--
repType :: Type -> RepType
repType ty
  = go initRecTc ty
  where
    go :: RecTcChecker -> Type -> RepType
    go rec_nts ty                       -- Expand predicates and synonyms
      | Just ty' <- coreView ty
      = go rec_nts ty'

    go rec_nts (ForAllTy _ ty2)         -- Drop type foralls
      = go rec_nts ty2

    go rec_nts ty@(TyConApp tc tys)     -- Expand newtypes
      | isNewTyCon tc
      , tys `lengthAtLeast` tyConArity tc
      , Just rec_nts' <- checkRecTc rec_nts tc   -- See Note [Expanding newtypes] in TyCon
      = go rec_nts' (newTyConInstRhs tc tys)

      | isUnboxedTupleTyCon tc
      = MultiRep (concatMap (repTypeSlots . go rec_nts) non_rr_tys)

      | isUnboxedSumTyCon tc
      = MultiRep (ubxSumRepType non_rr_tys)

      | isVoidTy ty
      = MultiRep []
      where
        -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
        non_rr_tys = dropRuntimeRepArgs tys

    go rec_nts (CastTy ty _)
      = go rec_nts ty

    go _ ty@(CoercionTy _)
      = pprPanic "repType" (ppr ty)

    go _ ty = UnaryRep ty


idFunRepArity :: Id -> RepArity
idFunRepArity x = countFunRepArgs (idArity x) (idType x)

countFunRepArgs :: Arity -> Type -> RepArity
countFunRepArgs 0 _
  = 0
countFunRepArgs n ty
  | UnaryRep (FunTy arg res) <- repType ty
  = length (repTypeArgs arg) + countFunRepArgs (n - 1) res
  | otherwise
  = pprPanic "countFunRepArgs: arity greater than type can handle" (ppr (n, ty, repType ty))

countConRepArgs :: DataCon -> RepArity
countConRepArgs dc = go (dataConRepArity dc) (dataConRepType dc)
  where
    go :: Arity -> Type -> RepArity
    go 0 _
      = 0
    go n ty
      | UnaryRep (FunTy arg res) <- repType ty
      = length (repTypeSlots (repType arg)) + go (n - 1) res
      | otherwise
      = pprPanic "countConRepArgs: arity greater than type can handle" (ppr (n, ty, repType ty))

-- | True if the type has zero width.
isVoidTy :: Type -> Bool
isVoidTy ty = typePrimRep ty == VoidRep


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
--   (# Int | Maybe Int | (# Int, Bool #) #)
--
-- We call `ubxSumRepType [ Int, Maybe Int, (# Int,Bool #) ]`,
-- which returns [Tag#, PtrSlot, PtrSlot]
--
-- INVARIANT: Result slots are sorted (via Ord SlotTy), except that at the head
-- of the list we have the slot for the tag.
ubxSumRepType :: [Type] -> [SlotTy]
ubxSumRepType constrs0 =
  ASSERT2( length constrs0 > 1, ppr constrs0 ) -- otherwise it isn't a sum type
  let
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
    rep :: Type -> SortedSlotTys
    rep ty = sort (repTypeSlots (repType ty))

    sumRep = WordSlot : combine_alts (map rep constrs0)
             -- WordSlot: for the tag of the sum
  in
    sumRep

layout :: SortedSlotTys -- Layout of sum. Does not include tag.
                        -- We assume that they are in increasing order
       -> [SlotTy]      -- Slot types of things we want to map to locations in the
                        -- sum layout
       -> [Int]         -- Where to map 'things' in the sum layout
layout sum_slots0 arg_slots0 =
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
  = Just (primRepSlot (typePrimRep ty))

primRepSlot :: PrimRep -> SlotTy
primRepSlot VoidRep     = pprPanic "primRepSlot" (text "No slot for VoidRep")
primRepSlot PtrRep      = PtrSlot
primRepSlot IntRep      = WordSlot
primRepSlot WordRep     = WordSlot
primRepSlot Int64Rep    = Word64Slot
primRepSlot Word64Rep   = Word64Slot
primRepSlot AddrRep     = WordSlot
primRepSlot FloatRep    = FloatSlot
primRepSlot DoubleRep   = DoubleSlot
primRepSlot VecRep{}    = pprPanic "primRepSlot" (text "No slot for VecRep")

-- Used when unarising sum binders (need to give unarised Ids types)
slotTyToType :: SlotTy -> Type
slotTyToType PtrSlot    = anyTypeOfKind liftedTypeKind
slotTyToType Word64Slot = int64PrimTy
slotTyToType WordSlot   = intPrimTy
slotTyToType DoubleSlot = doublePrimTy
slotTyToType FloatSlot  = floatPrimTy

slotPrimRep :: SlotTy -> PrimRep
slotPrimRep PtrSlot     = PtrRep
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

-- | Discovers the primitive representation of a more abstract 'UnaryType'
typePrimRep :: HasDebugCallStack => UnaryType -> PrimRep
typePrimRep ty = kindPrimRep (text "kindRep ty" <+> ppr ty $$ ppr (typeKind ty))
                             (typeKind ty)

-- | Find the runtime representation of a 'TyCon'. Defined here to
-- avoid module loops. Do not call this on unboxed tuples or sums,
-- because they don't /have/ a runtime representation
tyConPrimRep :: HasDebugCallStack => TyCon -> PrimRep
tyConPrimRep tc
  = ASSERT2( not (isUnboxedTupleTyCon tc), ppr tc )
    ASSERT2( not (isUnboxedSumTyCon   tc), ppr tc )
    kindPrimRep (text "kindRep tc" <+> ppr tc $$ ppr res_kind)
                res_kind
  where
    res_kind = tyConResKind tc

-- | Take a kind (of shape @TYPE rr@) and produce the 'PrimRep'
-- of values of types of this kind.
kindPrimRep :: HasDebugCallStack => SDoc -> Kind -> PrimRep
kindPrimRep doc ki
  | Just ki' <- coreViewOneStarKind ki
  = kindPrimRep doc ki'
kindPrimRep _ (TyConApp typ [runtime_rep])
  = ASSERT( typ `hasKey` tYPETyConKey )
    go runtime_rep
  where
    go rr
      | Just rr' <- coreView rr
      = go rr'
    go (TyConApp rr_dc args)
      | RuntimeRep fun <- tyConRuntimeRepInfo rr_dc
      = fun args
    go rr
      = pprPanic "kindPrimRep.go" (ppr rr)
kindPrimRep doc ki
  = WARN( True, text "kindPrimRep defaulting to PtrRep on" <+> ppr ki $$ doc )
    PtrRep  -- this can happen legitimately for, e.g., Any
