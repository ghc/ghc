
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module GHC.Types.Rep.Virtual
  (
    isVirtualTyCon, isVirtualDataCon, virtualDataConType, VirtualConType(..)
  ) where

import GHC.Prelude

import GHC.Types.RepType
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type

import GHC.Utils.Misc
import GHC.Utils.Outputable

import GHC.Utils.Trace
import GHC.Builtin.Types.Prim

{- **********************************************************************
*                                                                       *
                Virtual Data Con stuff
*                                                                       *
********************************************************************** -}

data VirtualConType = VirtualBoxed -- ^ These have a regular pointer tag
                    | VirtualUnboxedHeap -- ^ ByteArray# and friends. These don't usually have pointers.
                    | NonVirtual -- ^ Can't be shorted out.
                    deriving (Eq,Show)

instance Outputable VirtualConType where
  ppr :: VirtualConType -> SDoc
  ppr = text . show

isVirtualDataCon :: DataCon -> Bool
isVirtualDataCon con = virtualDataConType con /= NonVirtual

virtualDataConType :: DataCon -> VirtualConType
virtualDataConType = isVirtualTyCon . dataConTyCon

-- Is this unlifted type a fixed unboxed heap object type
isUnboxedVirtualTyCon :: TyCon -> Bool
isUnboxedVirtualTyCon tc
  | tc `elem`
    -- These are heap objects who naturally have pointer tag zero.
    [ arrayPrimTyCon
    , byteArrayPrimTyCon
    , smallArrayPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , smallMutableArrayPrimTyCon
    , mVarPrimTyCon
    , ioPortPrimTyCon
    , tVarPrimTyCon
    , mutVarPrimTyCon

    -- For these below I'm not sure about
    -- their representation.
    -- , weakPrimTyCon
    -- , stablePtrPrimTyCon
    -- , stableNamePrimTyCon
    -- , compactPrimTyCon
    -- , stackSnapshotPrimTyCon
    -- , promptTagPrimTyCon

    -- , tYPETyCon
    -- , funTyCon
    ]
  = True
  | otherwise = False

isVirtualTyCon :: HasDebugCallStack => TyCon -> VirtualConType
isVirtualTyCon tc
  -- Exactly one constructor
  | [dc] <- tyConDataCons tc
  -- No (runtime) constraints
  , [] <- filter (not . isZeroBitTy) (dataConOtherTheta dc)
  -- , pprTrace "isV.2" (ppr dc <> text ":" <> ppr tc) True
  --Exactly one non-void field argument
  , rep_bangs <- dataConRepStrictness dc
  , rep_tys <- dataConRepArgTys dc
  , all (tyHasFixedRuntimeRep) $ map scaledThing rep_tys
  -- , pprTrace "args,bangs" (ppr rep_bangs <> ppr rep_tys) True
  , [(field :: Type, strictness)] <- filter (not . isZeroBitTy . fst) $
                             zipWithEqual "isVirtualTyCon" (\a b -> (scaledThing a, b))
                             (rep_tys) (rep_bangs)
  , pprTrace "isV.3" empty True
  -- That field is boxed
  , isBoxedType field
  , pprTrace "isV.4" empty True
  -- And it's a boxed ADT!
  , isBoxedType (dataConOrigResTy dc)
  -- The field is either unlifted boxed or strict
  = if (isUnliftedType field)
      then
        isSafeUnlifted field
      else
        isSafeLifted strictness
  -- , pprTrace "isV.7" empty True
  -- -- Result is boxed
  -- = pprTrace "foundVirtualCon:" (ppr dc <> text ":" <> ppr tc <> text "@" <> ppr field) True
  | otherwise = NonVirtual
  where
    isSafeLifted strictness = case strictness of MarkedStrict -> pprTrace "safeBoxed" (ppr tc)  VirtualBoxed; _ -> NonVirtual

    isSafeUnlifted field
      | Just field_tc <- tyConAppTyCon_maybe field
      -- , pprTrace "ftc" (ppr field_tc) True
      , isDataTyCon field_tc
      = pprTrace "safeBoxedU" (ppr tc) VirtualBoxed
      | Just field_tc <- tyConAppTyCon_maybe field
      , isUnboxedVirtualTyCon field_tc
      = pprTrace "safeUnboxed" (ppr tc) VirtualUnboxedHeap
      -- = NonVirtual
      -- TODO: Hashmaps etc.
      | otherwise = NonVirtual
