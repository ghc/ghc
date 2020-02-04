{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- | Types for the Constructed Product Result lattice. "CprAnal" and "WwLib"
-- are its primary customers via 'idCprInfo'.
module Cpr (
    CprResult, topCpr, botCpr, sumCpr, prodCpr, returnsCPR_maybe,
    CprType (..), topCprType, botCprType, prodCprType, sumCprType,
    lubCprType, applyCprTy, abstractCprTy, ensureCprTyArity, trimCprTy,
    CprSig (..), topCprSig, mkCprSigForArity, mkCprSig, seqCprSig
  ) where

import GhcPrelude

import BasicTypes
import Outputable
import Binary

--
-- * CprResult
--

-- | The constructed product result lattice.
--
-- @
--                    NoCPR
--                    /    \
--             RetProd    RetSum ConTag
--                    \    /
--                    BotCPR
-- @
data CprResult = NoCPR          -- ^ Top of the lattice
               | RetProd        -- ^ Returns a constructor from a product type
               | RetSum !ConTag -- ^ Returns a constructor from a data type
               | BotCPR         -- ^ Bottom of the lattice
               deriving( Eq, Show )

lubCpr :: CprResult -> CprResult -> CprResult
lubCpr (RetSum t1) (RetSum t2)
  | t1 == t2               = RetSum t1
lubCpr RetProd     RetProd = RetProd
lubCpr BotCPR      cpr     = cpr
lubCpr cpr         BotCPR  = cpr
lubCpr _           _       = NoCPR

topCpr :: CprResult
topCpr = NoCPR

botCpr :: CprResult
botCpr = BotCPR

sumCpr :: ConTag -> CprResult
sumCpr = RetSum

prodCpr :: CprResult
prodCpr = RetProd

trimCpr :: Bool -> Bool -> CprResult -> CprResult
trimCpr trim_all trim_sums RetSum{}
  | trim_all || trim_sums      = NoCPR
trimCpr trim_all _         RetProd
  | trim_all                   = NoCPR
trimCpr _        _         cpr = cpr

returnsCPR_maybe :: CprResult -> Maybe ConTag
returnsCPR_maybe (RetSum t)  = Just t
returnsCPR_maybe RetProd     = Just fIRST_TAG
returnsCPR_maybe NoCPR       = Nothing
returnsCPR_maybe BotCPR      = Nothing

--
-- * CprType
--

-- | The abstract domain \(A_t\) from the original 'CPR for Haskell' paper.
data CprType
  = CprType
  { ct_arty :: !Arity     -- ^ Number of arguments the denoted expression eats
                          --   before returning the 'ct_cpr'
  , ct_cpr  :: !CprResult -- ^ 'CprResult' eventually unleashed when applied to
                          --   'ct_arty' arguments
  }

instance Eq CprType where
  a == b =  ct_cpr a == ct_cpr b
         && (ct_arty a == ct_arty b || ct_cpr a == topCpr)

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
  | cpr1 == botCpr && n1 <= n2 = ty2
  | cpr2 == botCpr && n2 <= n1 = ty1
  -- There might be non-bottom CPR types with mismatching arities.
  -- Consider test DmdAnalGADTs. We want to return top in these cases.
  | n1 == n2                   = CprType n1 (lubCpr cpr1 cpr2)
  | otherwise                  = topCprType

applyCprTy :: CprType -> CprType
applyCprTy (CprType n res)
  | n > 0         = CprType (n-1) res
  | res == botCpr = botCprType
  | otherwise     = topCprType

abstractCprTy :: CprType -> CprType
abstractCprTy (CprType n res)
  | res == topCpr = topCprType
  | otherwise     = CprType (n+1) res

ensureCprTyArity :: Arity -> CprType -> CprType
ensureCprTyArity n ty@(CprType m _)
  | n == m    = ty
  | otherwise = topCprType

trimCprTy :: Bool -> Bool -> CprType -> CprType
trimCprTy trim_all trim_sums (CprType arty res) = CprType arty (trimCpr trim_all trim_sums res)

-- | The arity of the wrapped 'CprType' is the arity at which it is safe
-- to unleash. See Note [Understanding DmdType and StrictSig] in Demand
newtype CprSig = CprSig { getCprSig :: CprType }
  deriving (Eq, Binary)

-- | Turns a 'CprType' computed for the particular 'Arity' into a 'CprSig'
-- unleashable at that arity. See Note [Understanding DmdType and StrictSig] in
-- Demand
mkCprSigForArity :: Arity -> CprType -> CprSig
mkCprSigForArity arty ty = CprSig (ensureCprTyArity arty ty)

topCprSig :: CprSig
topCprSig = CprSig topCprType

mkCprSig :: Arity -> CprResult -> CprSig
mkCprSig arty cpr = CprSig (CprType arty cpr)

seqCprSig :: CprSig -> ()
seqCprSig sig = sig `seq` ()

instance Outputable CprResult where
  ppr NoCPR        = empty
  ppr (RetSum n)   = char 'm' <> int n
  ppr RetProd      = char 'm'
  ppr BotCPR       = char 'b'

instance Outputable CprType where
  ppr (CprType arty res) = ppr arty <> ppr res

-- | Only print the CPR result
instance Outputable CprSig where
  ppr (CprSig ty) = ppr (ct_cpr ty)

instance Binary CprResult where
  put_ bh (RetSum n)   = do { putByte bh 0; put_ bh n }
  put_ bh RetProd      = putByte bh 1
  put_ bh NoCPR        = putByte bh 2
  put_ bh BotCPR       = putByte bh 3

  get  bh = do
          h <- getByte bh
          case h of
            0 -> do { n <- get bh; return (RetSum n) }
            1 -> return RetProd
            2 -> return NoCPR
            _ -> return BotCPR

instance Binary CprType where
  put_ bh (CprType arty cpr) = do
    put_ bh arty
    put_ bh cpr
  get  bh = CprType <$> get bh <*> get bh
