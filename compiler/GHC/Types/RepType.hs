
{-# LANGUAGE FlexibleContexts #-}

module GHC.Types.RepType
  (
    -- * Code generator views onto Types
    UnaryType, NvUnaryType, isNvUnaryType,
    unwrapType,

    -- * Predicates on types
    isZeroBitTy,

    -- * Type representation for the code generator
    typePrimRep, typePrimRep1,
    runtimeRepPrimRep, typePrimRepArgs,
    PrimRep(..), primRepToType,
    countFunRepArgs, countConRepArgs, dataConRuntimeRepStrictness,
    tyConPrimRep, tyConPrimRep1,
    runtimeRepPrimRep_maybe, kindPrimRep_maybe, typePrimRep_maybe,

    -- * Unboxed sum representation type
    ubxSumRepType, layoutUbxSum, typeSlotTy, SlotTy (..),
    slotPrimRep, primRepSlot,

    -- * Is this type known to be data?
    mightBeFunTy

    ) where

import GHC.Prelude

import GHC.Types.Basic (Arity, RepArity)
import GHC.Core.DataCon
import GHC.Builtin.Names
import GHC.Core.Coercion
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import {-# SOURCE #-} GHC.Builtin.Types ( anyTypeOfKind
  , vecRepDataConTyCon
  , liftedRepTy, unliftedRepTy, zeroBitRepTy
  , intRepDataConTy
  , int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy
  , wordRepDataConTy
  , word16RepDataConTy, word8RepDataConTy, word32RepDataConTy, word64RepDataConTy
  , addrRepDataConTy
  , floatRepDataConTy, doubleRepDataConTy
  , vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy
  , vec64DataConTy
  , int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy
  , int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy
  , word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy
  , doubleElemRepDataConTy )

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

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
  | FunTy _ _ arg res <- unwrapType ty
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
      | FunTy _ _ arg res <- unwrapType ty
      = length (typePrimRep arg) + go (n - 1) res
      | otherwise
      = pprPanic "countConRepArgs: arity greater than type can handle" (ppr (n, ty, typePrimRep ty))

dataConRuntimeRepStrictness :: HasDebugCallStack => DataCon -> [StrictnessMark]
-- ^ Give the demands on the arguments of a
-- Core constructor application (Con dc args) at runtime.
-- Assumes the constructor is not levity polymorphic. For example
-- unboxed tuples won't work.
dataConRuntimeRepStrictness dc =

  -- pprTrace "dataConRuntimeRepStrictness" (ppr dc $$ ppr (dataConRepArgTys dc)) $

  let repMarks = dataConRepStrictness dc
      repTys = map irrelevantMult $ dataConRepArgTys dc
  in -- todo: assert dc != unboxedTuple/unboxedSum
     go repMarks repTys []
  where
    go (mark:marks) (ty:types) out_marks
      -- Zero-width argument, mark is irrelevant at runtime.
      |  -- pprTrace "VoidTy" (ppr ty) $
        (isZeroBitTy ty)
      = go marks types out_marks
      -- Single rep argument, e.g. Int
      -- Keep mark as-is
      | [_] <- reps
      = go marks types (mark:out_marks)
      -- Multi-rep argument, e.g. (# Int, Bool #) or (# Int | Bool #)
      -- Make up one non-strict mark per runtime argument.
      | otherwise -- TODO: Assert real_reps /= null
      = go marks types ((replicate (length real_reps) NotMarkedStrict)++out_marks)
      where
        reps = typePrimRep ty
        real_reps = filter (not . isVoidRep) $ reps
    go [] [] out_marks = reverse out_marks
    go _m _t _o = pprPanic "dataConRuntimeRepStrictness2" (ppr dc $$ ppr _m $$ ppr _t $$ ppr _o)

-- | True if the type has zero width.
isZeroBitTy :: HasDebugCallStack => Type -> Bool
isZeroBitTy = null . typePrimRep


{- **********************************************************************
*                                                                       *
                Unboxed sums
 See Note [Translating unboxed sums to unboxed tuples] in GHC.Stg.Unarise
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
--     boxed objects). These come in two variants: Lifted and unlifted (see
--     #19645).
--
--   - Word slots: Shared between IntRep, WordRep, Int64Rep, Word64Rep, AddrRep.
--
--   - Float slots: Shared between floating point types.
--
--   - Void slots: Shared between void types. Not used in sums.
--
-- TODO(michalt): We should probably introduce `SlotTy`s for 8-/16-/32-bit
-- values, so that we can pack things more tightly.
data SlotTy = PtrLiftedSlot | PtrUnliftedSlot | WordSlot | Word64Slot | FloatSlot | DoubleSlot
  deriving (Eq, Ord)
    -- Constructor order is important! If slot A could fit into slot B
    -- then slot A must occur first.  E.g.  FloatSlot before DoubleSlot
    --
    -- We are assuming that WordSlot is smaller than or equal to Word64Slot
    -- (would not be true on a 128-bit machine)

instance Outputable SlotTy where
  ppr PtrLiftedSlot   = text "PtrLiftedSlot"
  ppr PtrUnliftedSlot = text "PtrUnliftedSlot"
  ppr Word64Slot      = text "Word64Slot"
  ppr WordSlot        = text "WordSlot"
  ppr DoubleSlot      = text "DoubleSlot"
  ppr FloatSlot       = text "FloatSlot"

typeSlotTy :: UnaryType -> Maybe SlotTy
typeSlotTy ty
  | isZeroBitTy ty
  = Nothing
  | otherwise
  = Just (primRepSlot (typePrimRep1 ty))

primRepSlot :: PrimRep -> SlotTy
primRepSlot VoidRep     = pprPanic "primRepSlot" (text "No slot for VoidRep")
primRepSlot LiftedRep   = PtrLiftedSlot
primRepSlot UnliftedRep = PtrUnliftedSlot
primRepSlot IntRep      = WordSlot
primRepSlot Int8Rep     = WordSlot
primRepSlot Int16Rep    = WordSlot
primRepSlot Int32Rep    = WordSlot
primRepSlot Int64Rep    = Word64Slot
primRepSlot WordRep     = WordSlot
primRepSlot Word8Rep    = WordSlot
primRepSlot Word16Rep   = WordSlot
primRepSlot Word32Rep   = WordSlot
primRepSlot Word64Rep   = Word64Slot
primRepSlot AddrRep     = WordSlot
primRepSlot FloatRep    = FloatSlot
primRepSlot DoubleRep   = DoubleSlot
primRepSlot VecRep{}    = pprPanic "primRepSlot" (text "No slot for VecRep")

slotPrimRep :: SlotTy -> PrimRep
slotPrimRep PtrLiftedSlot   = LiftedRep
slotPrimRep PtrUnliftedSlot = UnliftedRep
slotPrimRep Word64Slot      = Word64Rep
slotPrimRep WordSlot        = WordRep
slotPrimRep DoubleSlot      = DoubleRep
slotPrimRep FloatSlot       = FloatRep

-- | Returns the bigger type if one fits into the other. (commutative)
--
-- Note that lifted and unlifted pointers are *not* in a fits-in relation for
-- the reasons described in Note [Don't merge lifted and unlifted slots] in
-- GHC.Stg.Unarise.
fitsIn :: SlotTy -> SlotTy -> Maybe SlotTy
fitsIn ty1 ty2
  | ty1 == ty2
  = Just ty1
  | isWordSlot ty1 && isWordSlot ty2
  = Just (max ty1 ty2)
  | isFloatSlot ty1 && isFloatSlot ty2
  = Just (max ty1 ty2)
  | otherwise
  = Nothing
  where
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
*************************************************************************

Note [RuntimeRep and PrimRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes the relationship between GHC.Types.RuntimeRep
(of levity/representation polymorphism fame) and GHC.Core.TyCon.PrimRep,
as these types are closely related.

A "primitive entity" is one that can be
 * stored in one register
 * manipulated with one machine instruction


Examples include:
 * a 32-bit integer
 * a 32-bit float
 * a 64-bit float
 * a machine address (heap pointer), etc.
 * a quad-float (on a machine with SIMD register and instructions)
 * ...etc...

The "representation or a primitive entity" specifies what kind of register is
needed and how many bits are required. The data type GHC.Core.TyCon.PrimRep
enumerates all the possibilities.

data PrimRep
  = VoidRep       -- See Note [VoidRep]
  | LiftedRep     -- ^ Lifted pointer
  | UnliftedRep   -- ^ Unlifted pointer
  | Int8Rep       -- ^ Signed, 8-bit value
  | Int16Rep      -- ^ Signed, 16-bit value
  ...etc...
  | VecRep Int PrimElemRep  -- ^ SIMD fixed-width vector

The Haskell source language is a bit more flexible: a single value may need multiple PrimReps.
For example

  utup :: (# Int, Int #) -> Bool
  utup x = ...

Here x :: (# Int, Int #), and that takes two registers, and two instructions to move around.
Unboxed sums are similar.

Every Haskell expression e has a type ty, whose kind is of form TYPE rep
   e :: ty :: TYPE rep
where rep :: RuntimeRep. Here rep describes the runtime representation for e's value,
but RuntimeRep has some extra cases:

data RuntimeRep = VecRep VecCount VecElem   -- ^ a SIMD vector type
                | TupleRep [RuntimeRep]     -- ^ An unboxed tuple of the given reps
                | SumRep [RuntimeRep]       -- ^ An unboxed sum of the given reps
                | BoxedRep Levity -- ^ boxed; represented by a pointer
                | IntRep          -- ^ signed, word-sized value
                ...etc...
data Levity     = Lifted
                | Unlifted

It's all in 1-1 correspondence with PrimRep except for TupleRep and SumRep,
which describe unboxed products and sums respectively. RuntimeRep is defined
in the library ghc-prim:GHC.Types. It is also "wired-in" to GHC: see
GHC.Builtin.Types.runtimeRepTyCon. The unarisation pass, in GHC.Stg.Unarise, transforms the
program, so that every variable has a type that has a PrimRep. For
example, unarisation transforms our utup function above, to take two Int
arguments instead of one (# Int, Int #) argument.

Also, note that boxed types are represented slightly differently in RuntimeRep
and PrimRep. PrimRep just has the nullary LiftedRep and UnliftedRep data
constructors. RuntimeRep has a BoxedRep data constructor, which accepts a
Levity. The subtle distinction is that since BoxedRep can accept a variable
argument, RuntimeRep can talk about levity polymorphic types. PrimRep, by
contrast, cannot.

See also Note [Getting from RuntimeRep to PrimRep] and Note [VoidRep].

Note [VoidRep]
~~~~~~~~~~~~~~
PrimRep contains a constructor VoidRep, while RuntimeRep does
not. Yet representations are often characterised by a list of PrimReps,
where a void would be denoted as []. (See also Note [RuntimeRep and PrimRep].)

However, after the unariser, all identifiers have exactly one PrimRep, but
void arguments still exist. Thus, PrimRep includes VoidRep to describe these
binders. Perhaps post-unariser representations (which need VoidRep) should be
a different type than pre-unariser representations (which use a list and do
not need VoidRep), but we have what we have.

RuntimeRep instead uses TupleRep '[] to denote a void argument. When
converting a TupleRep '[] into a list of PrimReps, we get an empty list.

Note [Getting from RuntimeRep to PrimRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
General info on RuntimeRep and PrimRep is in Note [RuntimeRep and PrimRep].

How do we get from an Id to the list or PrimReps used to store it? We get
the Id's type ty (using idType), then ty's kind ki (using typeKind), then
pattern-match on ki to extract rep (in kindPrimRep), then extract the PrimRep
from the RuntimeRep (in runtimeRepPrimRep).

We now must convert the RuntimeRep to a list of PrimReps. Let's look at two
examples:

  1. x :: Int#
  2. y :: (# Int, Word# #)

With these types, we can extract these kinds:

  1. Int# :: TYPE IntRep
  2. (# Int, Word# #) :: TYPE (TupleRep [LiftedRep, WordRep])

In the end, we will get these PrimReps:

  1. [IntRep]
  2. [LiftedRep, WordRep]

It would thus seem that we should have a function somewhere of
type `RuntimeRep -> [PrimRep]`. This doesn't work though: when we
look at the argument of TYPE, we get something of type Type (of course).
RuntimeRep exists in the user's program, but not in GHC as such.
Instead, we must decompose the Type of kind RuntimeRep into tycons and
extract the PrimReps from the TyCons. This is what runtimeRepPrimRep does:
it takes a Type and returns a [PrimRep]

runtimeRepPrimRep works by using tyConRuntimeRepInfo. That function
should be passed the TyCon produced by promoting one of the constructors
of RuntimeRep into type-level data. The RuntimeRep promoted datacons are
associated with a RuntimeRepInfo (stored directly in the PromotedDataCon
constructor of TyCon, field promDcRepInfo).
This pairing happens in GHC.Builtin.Types. A RuntimeRepInfo
usually(*) contains a function from [Type] to [PrimRep]: the [Type] are
the arguments to the promoted datacon. These arguments are necessary
for the TupleRep and SumRep constructors, so that this process can recur,
producing a flattened list of PrimReps. Calling this extracted function
happens in runtimeRepPrimRep; the functions themselves are defined in
tupleRepDataCon and sumRepDataCon, both in GHC.Builtin.Types.

The (*) above is to support vector representations. RuntimeRep refers
to VecCount and VecElem, whose promoted datacons have nuggets of information
related to vectors; these form the other alternatives for RuntimeRepInfo.

Returning to our examples, the Types we get (after stripping off TYPE) are

  1. TyConApp (PromotedDataCon "IntRep") []
  2. TyConApp (PromotedDataCon "TupleRep")
              [TyConApp (PromotedDataCon ":")
                        [ TyConApp (AlgTyCon "RuntimeRep") []
                        , TyConApp (PromotedDataCon "LiftedRep") []
                        , TyConApp (PromotedDataCon ":")
                                   [ TyConApp (AlgTyCon "RuntimeRep") []
                                   , TyConApp (PromotedDataCon "WordRep") []
                                   , TyConApp (PromotedDataCon "'[]")
                                              [TyConApp (AlgTyCon "RuntimeRep") []]]]]

runtimeRepPrimRep calls tyConRuntimeRepInfo on (PromotedDataCon "IntRep"), resp.
(PromotedDataCon "TupleRep"), extracting a function that will produce the PrimReps.
In example 1, this function is passed an empty list (the empty list of args to IntRep)
and returns the PrimRep IntRep. (See the definition of runtimeRepSimpleDataCons in
GHC.Builtin.Types and its helper function mk_runtime_rep_dc.) Example 2 passes the promoted
list as the one argument to the extracted function. The extracted function is defined
as prim_rep_fun within tupleRepDataCon in GHC.Builtin.Types. It takes one argument, decomposes
the promoted list (with extractPromotedList), and then recurs back to runtimeRepPrimRep
to process the LiftedRep and WordRep, concatentating the results.

-}

-- | Discovers the primitive representation of a 'Type'. Returns
-- a list of 'PrimRep': it's a list because of the possibility of
-- no runtime representation (void) or multiple (unboxed tuple/sum)
-- See also Note [Getting from RuntimeRep to PrimRep]
typePrimRep :: HasDebugCallStack => Type -> [PrimRep]
typePrimRep ty = kindPrimRep (text "typePrimRep" <+>
                              parens (ppr ty <+> dcolon <+> ppr (typeKind ty)))
                             (typeKind ty)

-- | Discovers the primitive representation of a 'Type'. Returns
-- a list of 'PrimRep': it's a list because of the possibility of
-- no runtime representation (void) or multiple (unboxed tuple/sum)
-- See also Note [Getting from RuntimeRep to PrimRep]
-- Returns Nothing if rep can't be determined. Eg. levity polymorphic types.
typePrimRep_maybe :: Type -> Maybe [PrimRep]
typePrimRep_maybe ty = kindPrimRep_maybe (typeKind ty)

-- | Like 'typePrimRep', but assumes that there is precisely one 'PrimRep' output;
-- an empty list of PrimReps becomes a VoidRep.
-- This assumption holds after unarise, see Note [Post-unarisation invariants].
-- Before unarise it may or may not hold.
-- See also Note [RuntimeRep and PrimRep] and Note [VoidRep]
typePrimRep1 :: HasDebugCallStack => UnaryType -> PrimRep
typePrimRep1 ty = case typePrimRep ty of
  []    -> VoidRep
  [rep] -> rep
  _     -> pprPanic "typePrimRep1" (ppr ty $$ ppr (typePrimRep ty))

-- | Find the runtime representation of a 'TyCon'. Defined here to
-- avoid module loops. Returns a list of the register shapes necessary.
-- See also Note [Getting from RuntimeRep to PrimRep]
tyConPrimRep :: HasDebugCallStack => TyCon -> [PrimRep]
tyConPrimRep tc
  = kindPrimRep (text "kindRep tc" <+> ppr tc $$ ppr res_kind)
                res_kind
  where
    res_kind = tyConResKind tc

-- | Like 'tyConPrimRep', but assumed that there is precisely zero or
-- one 'PrimRep' output
-- See also Note [Getting from RuntimeRep to PrimRep] and Note [VoidRep]
tyConPrimRep1 :: HasDebugCallStack => TyCon -> PrimRep
tyConPrimRep1 tc = case tyConPrimRep tc of
  []    -> VoidRep
  [rep] -> rep
  _     -> pprPanic "tyConPrimRep1" (ppr tc $$ ppr (tyConPrimRep tc))

-- | Take a kind (of shape @TYPE rr@) and produce the 'PrimRep's
-- of values of types of this kind.
-- See also Note [Getting from RuntimeRep to PrimRep]
kindPrimRep :: HasDebugCallStack => SDoc -> Kind -> [PrimRep]
kindPrimRep doc ki
  | Just ki' <- coreView ki
  = kindPrimRep doc ki'
kindPrimRep doc (TyConApp typ [runtime_rep])
  = assert (typ `hasKey` tYPETyConKey) $
    runtimeRepPrimRep doc runtime_rep
kindPrimRep doc ki
  = pprPanic "kindPrimRep" (ppr ki $$ doc)

-- NB: We could implement the partial methods by calling into the maybe
-- variants here. But then both would need to pass around the doc argument.

-- | Take a kind (of shape @TYPE rr@) and produce the 'PrimRep's
-- of values of types of this kind.
-- See also Note [Getting from RuntimeRep to PrimRep]
-- Returns Nothing if rep can't be determined. Eg. levity polymorphic types.
kindPrimRep_maybe :: HasDebugCallStack => Kind -> Maybe [PrimRep]
kindPrimRep_maybe ki
  | Just ki' <- coreView ki
  = kindPrimRep_maybe ki'
kindPrimRep_maybe (TyConApp typ [runtime_rep])
  = assert (typ `hasKey` tYPETyConKey) $
    runtimeRepPrimRep_maybe runtime_rep
kindPrimRep_maybe _ki
  = Nothing

-- | Take a type of kind RuntimeRep and extract the list of 'PrimRep' that
-- it encodes. See also Note [Getting from RuntimeRep to PrimRep]
-- The [PrimRep] is the final runtime representation /after/ unarisation
runtimeRepPrimRep :: HasDebugCallStack => SDoc -> Type -> [PrimRep]
runtimeRepPrimRep doc rr_ty
  | Just rr_ty' <- coreView rr_ty
  = runtimeRepPrimRep doc rr_ty'
  | TyConApp rr_dc args <- rr_ty
  , RuntimeRep fun <- tyConRuntimeRepInfo rr_dc
  = fun args
  | otherwise
  = pprPanic "runtimeRepPrimRep" (doc $$ ppr rr_ty)

-- | Take a type of kind RuntimeRep and extract the list of 'PrimRep' that
-- it encodes. See also Note [Getting from RuntimeRep to PrimRep]
-- The [PrimRep] is the final runtime representation /after/ unarisation
-- Returns Nothing if rep can't be determined. Eg. levity polymorphic types.
runtimeRepPrimRep_maybe :: Type -> Maybe [PrimRep]
runtimeRepPrimRep_maybe rr_ty
  | Just rr_ty' <- coreView rr_ty
  = runtimeRepPrimRep_maybe rr_ty'
  | TyConApp rr_dc args <- rr_ty
  , RuntimeRep fun <- tyConRuntimeRepInfo rr_dc
  = Just $! fun args
  | otherwise
  = Nothing

-- | Convert a 'PrimRep' to a 'Type' of kind RuntimeRep
primRepToRuntimeRep :: PrimRep -> Type
primRepToRuntimeRep rep = case rep of
  VoidRep       -> zeroBitRepTy
  LiftedRep     -> liftedRepTy
  UnliftedRep   -> unliftedRepTy
  IntRep        -> intRepDataConTy
  Int8Rep       -> int8RepDataConTy
  Int16Rep      -> int16RepDataConTy
  Int32Rep      -> int32RepDataConTy
  Int64Rep      -> int64RepDataConTy
  WordRep       -> wordRepDataConTy
  Word8Rep      -> word8RepDataConTy
  Word16Rep     -> word16RepDataConTy
  Word32Rep     -> word32RepDataConTy
  Word64Rep     -> word64RepDataConTy
  AddrRep       -> addrRepDataConTy
  FloatRep      -> floatRepDataConTy
  DoubleRep     -> doubleRepDataConTy
  VecRep n elem -> TyConApp vecRepDataConTyCon [n', elem']
    where
      n' = case n of
        2  -> vec2DataConTy
        4  -> vec4DataConTy
        8  -> vec8DataConTy
        16 -> vec16DataConTy
        32 -> vec32DataConTy
        64 -> vec64DataConTy
        _  -> pprPanic "Disallowed VecCount" (ppr n)

      elem' = case elem of
        Int8ElemRep   -> int8ElemRepDataConTy
        Int16ElemRep  -> int16ElemRepDataConTy
        Int32ElemRep  -> int32ElemRepDataConTy
        Int64ElemRep  -> int64ElemRepDataConTy
        Word8ElemRep  -> word8ElemRepDataConTy
        Word16ElemRep -> word16ElemRepDataConTy
        Word32ElemRep -> word32ElemRepDataConTy
        Word64ElemRep -> word64ElemRepDataConTy
        FloatElemRep  -> floatElemRepDataConTy
        DoubleElemRep -> doubleElemRepDataConTy

-- | Convert a PrimRep back to a Type. Used only in the unariser to give types
-- to fresh Ids. Really, only the type's representation matters.
-- See also Note [RuntimeRep and PrimRep]
primRepToType :: PrimRep -> Type
primRepToType = anyTypeOfKind . mkTYPEapp . primRepToRuntimeRep

--------------
mightBeFunTy :: Type -> Bool
-- Return False only if we are *sure* it's a data type
-- Look through newtypes etc as much as possible. Used to
-- decide if we need to enter a closure via a slow call.
--
-- AK: It would be nice to figure out and document the difference
-- between this and isFunTy at some point.
mightBeFunTy ty
  | [LiftedRep] <- typePrimRep ty
  , Just tc <- tyConAppTyCon_maybe (unwrapType ty)
  , isDataTyCon tc
  = False
  | otherwise
  = True
