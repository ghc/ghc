module Cpr (
    CprType (..), topCprType, botCprType, prodCprType, sumCprType,
    lubCprType, applyCprTy, abstractCprTy, ensureCprTyArity, trimCprTy
  ) where

import GhcPrelude

import BasicTypes
import Demand
import Outputable

-- | The abstract domain $A_t$ from the original 'CPR for Haskell' paper.
data CprType
  = CprType
  { _ct_arty :: !Arity    -- ^ Number of arguments the denoted expression eats
                          --   before returning the 'ct_cpr'
  , ct_cpr  :: !CPRResult -- ^ 'CPRResult' eventually unleashed when applied to
                          --   'ct_arty' arguments
  }

instance Eq CprType where
  a == b =  ct_cpr a == ct_cpr b
         && (_ct_arty a == _ct_arty b || ct_cpr a == topCpr)

topCprType :: CprType
topCprType = CprType 0 topCpr

botCprType :: CprType
botCprType = CprType 0 botCpr -- TODO: Figure out if arity 0 does what we want... Yes it does: arity zero means we may unleash it under any number of incoming arguments

prodCprType :: Arity -> CprType
prodCprType _con_arty = CprType 0 prodCpr

sumCprType :: ConTag -> CprType
sumCprType con_tag = CprType 0 (sumCpr con_tag)

lubCprType :: CprType -> CprType -> CprType
lubCprType ty1@(CprType n1 cpr1) ty2@(CprType n2 cpr2)
  -- The arity of bottom CPR types can be extended arbitrarily.
  | isBotCpr cpr1 && n1 <= n2 = ty2
  | isBotCpr cpr2 && n2 <= n1 = ty1
  -- There might be non-bottom CPR types with mismatching arities.
  -- Consider test DmdAnalGADTs. We want to return top in these cases.
  | n1 == n2                  = CprType n1 (lubCPR cpr1 cpr2)
  | otherwise                 = topCprType

applyCprTy :: CprType -> CprType
applyCprTy (CprType n res)
  | n > 0        = CprType (n-1) res
  | isBotCpr res = botCprType
  | otherwise    = topCprType

abstractCprTy :: CprType -> CprType
abstractCprTy (CprType n res)
  | isTopCpr res = topCprType
  | otherwise    = CprType (n+1) res

ensureCprTyArity :: Arity -> CprType -> CprType
ensureCprTyArity n ty@(CprType m _)
  | n == m    = ty
  | otherwise = topCprType

trimCprTy :: Bool -> Bool -> CprType -> CprType
trimCprTy trim_all trim_sums (CprType arty res) = CprType arty (trimCpr trim_all trim_sums res)

instance Outputable CprType where
  ppr (CprType arty res) = ppr arty <> ppr res
