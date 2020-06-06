{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- | Types for the Constructed Product Result lattice. "GHC.Core.Opt.CprAnal" and "GHC.Core.Opt.WorkWrap.Utils"
-- are its primary customers via 'GHC.Types.Id.idCprInfo'.
module GHC.Types.Cpr (
    CprResult, topCpr, botCpr, conCpr, asConCpr,
    CprType (..), topCprType, botCprType, conCprType,
    lubCprType, applyCprTy, abstractCprTy, ensureCprTyArity, trimCprTy,
    CprSig (..), topCprSig, mkCprSigForArity, mkCprSig, seqCprSig
  ) where

import GHC.Prelude

import GHC.Types.Basic
import GHC.Utils.Outputable
import GHC.Utils.Binary

--
-- * CprResult
--

-- | The constructed product result lattice.
--
-- @
--                    NoCPR
--                      |
--                 ConCPR ConTag
--                      |
--                    BotCPR
-- @
data CprResult = NoCPR          -- ^ Top of the lattice
               | ConCPR !ConTag -- ^ Returns a constructor from a data type
               | BotCPR         -- ^ Bottom of the lattice
               deriving( Eq, Show )

lubCpr :: CprResult -> CprResult -> CprResult
lubCpr (ConCPR t1) (ConCPR t2)
  | t1 == t2               = ConCPR t1
lubCpr BotCPR      cpr     = cpr
lubCpr cpr         BotCPR  = cpr
lubCpr _           _       = NoCPR

topCpr :: CprResult
topCpr = NoCPR

botCpr :: CprResult
botCpr = BotCPR

conCpr :: ConTag -> CprResult
conCpr = ConCPR

trimCpr :: CprResult -> CprResult
trimCpr ConCPR{} = NoCPR
trimCpr cpr      = cpr

asConCpr :: CprResult -> Maybe ConTag
asConCpr (ConCPR t)  = Just t
asConCpr NoCPR       = Nothing
asConCpr BotCPR      = Nothing

--
-- * CprType
--

-- | The abstract domain \(A_t\) from the original 'CPR for Haskell' paper.
data CprType
  = CprType
  { ct_arty :: !Arity     -- ^ Number of value arguments the denoted expression
                          --   eats before returning the 'ct_cpr'
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

conCprType :: ConTag -> CprType
conCprType con_tag = CprType 0 (conCpr con_tag)

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

trimCprTy :: CprType -> CprType
trimCprTy (CprType arty res) = CprType arty (trimCpr res)

-- | The arity of the wrapped 'CprType' is the arity at which it is safe
-- to unleash. See Note [Understanding DmdType and StrictSig] in "GHC.Types.Demand"
newtype CprSig = CprSig { getCprSig :: CprType }
  deriving (Eq, Binary)

-- | Turns a 'CprType' computed for the particular 'Arity' into a 'CprSig'
-- unleashable at that arity. See Note [Understanding DmdType and StrictSig] in
-- "GHC.Types.Demand"
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
  ppr (ConCPR n)   = char 'm' <> int n
  ppr BotCPR       = char 'b'

instance Outputable CprType where
  ppr (CprType arty res) = ppr arty <> ppr res

-- | Only print the CPR result
instance Outputable CprSig where
  ppr (CprSig ty) = ppr (ct_cpr ty)

instance Binary CprResult where
  put_ bh (ConCPR n)   = do { putByte bh 0; put_ bh n }
  put_ bh NoCPR        = putByte bh 1
  put_ bh BotCPR       = putByte bh 2

  get  bh = do
          h <- getByte bh
          case h of
            0 -> do { n <- get bh; return (ConCPR n) }
            1 -> return NoCPR
            _ -> return BotCPR

instance Binary CprType where
  put_ bh (CprType arty cpr) = do
    put_ bh arty
    put_ bh cpr
  get  bh = CprType <$> get bh <*> get bh
