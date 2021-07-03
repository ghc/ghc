{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Types for the Constructed Product Result lattice.
-- "GHC.Core.Opt.CprAnal" and "GHC.Core.Opt.WorkWrap.Utils"
-- are its primary customers via 'GHC.Types.Id.idCprInfo'.
module GHC.Types.Cpr (
    Cpr (ConCpr), topCpr, botCpr, flatConCpr, asConCpr,
    CprType (..), topCprType, botCprType, flatConCprType,
    lubCprType, applyCprTy, abstractCprTy, trimCprTy,
    UnpackConFieldsResult (..), unpackConFieldsCpr,
    CprSig (..), topCprSig, isTopCprSig, mkCprSigForArity, mkCprSig, seqCprSig
  ) where

import GHC.Prelude

import GHC.Core.DataCon
import GHC.Types.Basic
import GHC.Utils.Binary
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

--
-- * Cpr
--

data Cpr
  = BotCpr
  | ConCpr_ !ConTag ![Cpr]
  -- ^ The number of field Cprs equals 'dataConRepArity'.
  -- If all of them are top, better use 'FlatConCpr', as ensured by the pattern
  -- synonym 'ConCpr'.
  | FlatConCpr !ConTag
  | TopCpr
  deriving Eq

pattern ConCpr :: ConTag -> [Cpr] -> Cpr
pattern ConCpr t cs <- ConCpr_ t cs where
  ConCpr t cs
    | all (== TopCpr) cs = FlatConCpr t
    | otherwise          = ConCpr_ t cs
{-# COMPLETE BotCpr, TopCpr, FlatConCpr, ConCpr #-}

viewConTag :: Cpr -> Maybe ConTag
viewConTag (FlatConCpr t) = Just t
viewConTag (ConCpr t _)   = Just t
viewConTag _              = Nothing
{-# INLINE viewConTag #-}

lubCpr :: Cpr -> Cpr -> Cpr
lubCpr BotCpr      cpr     = cpr
lubCpr cpr         BotCpr  = cpr
lubCpr (FlatConCpr t1) (viewConTag -> Just t2)
  | t1 == t2 = FlatConCpr t1
lubCpr (viewConTag -> Just t1) (FlatConCpr t2)
  | t1 == t2 = FlatConCpr t2
lubCpr (ConCpr t1 cs1) (ConCpr t2 cs2)
  | t1 == t2 = ConCpr t1 (lubFieldCprs cs1 cs2)
lubCpr _           _       = TopCpr

lubFieldCprs :: [Cpr] -> [Cpr] -> [Cpr]
lubFieldCprs as bs
  | as `equalLength` bs = zipWith lubCpr as bs
  | otherwise           = []

topCpr :: Cpr
topCpr = TopCpr

botCpr :: Cpr
botCpr = BotCpr

flatConCpr :: ConTag -> Cpr
flatConCpr t = FlatConCpr t

trimCpr :: Cpr -> Cpr
trimCpr BotCpr = botCpr
trimCpr _      = topCpr

asConCpr :: Cpr -> Maybe (ConTag, [Cpr])
asConCpr (ConCpr t cs)  = Just (t, cs)
asConCpr (FlatConCpr t) = Just (t, [])
asConCpr TopCpr         = Nothing
asConCpr BotCpr         = Nothing

seqCpr :: Cpr -> ()
seqCpr (ConCpr _ cs) = foldr (seq . seqCpr) () cs
seqCpr _             = ()

--
-- * CprType
--

-- | The abstract domain \(A_t\) from the original 'CPR for Haskell' paper.
data CprType
  = CprType
  { ct_arty :: !Arity -- ^ Number of value arguments the denoted expression
                      --   eats before returning the 'ct_cpr'
  , ct_cpr  :: !Cpr   -- ^ 'Cpr' eventually unleashed when applied to
                      --   'ct_arty' arguments
  }

instance Eq CprType where
  a == b =  ct_cpr a == ct_cpr b
         && (ct_arty a == ct_arty b || ct_cpr a == topCpr)

topCprType :: CprType
topCprType = CprType 0 topCpr

botCprType :: CprType
botCprType = CprType 0 botCpr

flatConCprType :: ConTag -> CprType
flatConCprType con_tag = CprType { ct_arty = 0, ct_cpr = flatConCpr con_tag }

lubCprType :: CprType -> CprType -> CprType
lubCprType ty1@(CprType n1 cpr1) ty2@(CprType n2 cpr2)
  -- The arity of bottom CPR types can be extended arbitrarily.
  | cpr1 == botCpr && n1 <= n2 = ty2
  | cpr2 == botCpr && n2 <= n1 = ty1
  -- There might be non-bottom CPR types with mismatching arities.
  -- Consider test DmdAnalGADTs. We want to return top in these cases.
  | n1 == n2                   = CprType n1 (lubCpr cpr1 cpr2)
  | otherwise                  = topCprType

applyCprTy :: CprType -> Arity -> CprType
applyCprTy (CprType n res) k
  | n >= k        = CprType (n-k) res
  | res == botCpr = botCprType
  | otherwise     = topCprType

abstractCprTy :: CprType -> CprType
abstractCprTy (CprType n res)
  | res == topCpr = topCprType
  | otherwise     = CprType (n+1) res

trimCprTy :: CprType -> CprType
trimCprTy (CprType arty res) = CprType arty (trimCpr res)

-- | The result of 'unpackConFieldsCpr'.
data UnpackConFieldsResult
  = AllFieldsSame !Cpr
  | ForeachField ![Cpr]

-- | Unpacks a 'ConCpr'-shaped 'Cpr' and returns the field 'Cpr's wrapped in a
-- 'ForeachField'. Otherwise, it returns 'AllFieldsSame' with the appropriate
-- 'Cpr' to assume for each field.
--
-- The use of 'UnpackConFieldsResult' allows O(1) space for the common,
-- non-'ConCpr' case.
unpackConFieldsCpr :: DataCon -> Cpr -> UnpackConFieldsResult
unpackConFieldsCpr dc (ConCpr t cs)
  | t == dataConTag dc, cs `lengthIs` dataConRepArity dc
  = ForeachField cs
unpackConFieldsCpr _  BotCpr = AllFieldsSame BotCpr
unpackConFieldsCpr _  _      = AllFieldsSame TopCpr
{-# INLINE unpackConFieldsCpr #-}

seqCprTy :: CprType -> ()
seqCprTy (CprType _ cpr) = seqCpr cpr

-- | The arity of the wrapped 'CprType' is the arity at which it is safe
-- to unleash. See Note [Understanding DmdType and StrictSig] in "GHC.Types.Demand"
newtype CprSig = CprSig { getCprSig :: CprType }
  deriving (Eq, Binary)

-- | Turns a 'CprType' computed for the particular 'Arity' into a 'CprSig'
-- unleashable at that arity. See Note [Understanding DmdType and StrictSig] in
-- "GHC.Types.Demand"
mkCprSigForArity :: Arity -> CprType -> CprSig
mkCprSigForArity arty ty@(CprType n cpr)
  | arty /= n         = topCprSig
      -- Trim on arity mismatch
  | ConCpr t _ <- cpr = CprSig (CprType n (flatConCpr t))
      -- Flatten nested CPR info, we don't exploit it (yet)
  | otherwise         = CprSig ty

topCprSig :: CprSig
topCprSig = CprSig topCprType

isTopCprSig :: CprSig -> Bool
isTopCprSig (CprSig ty) = ct_cpr ty == topCpr

mkCprSig :: Arity -> Cpr -> CprSig
mkCprSig arty cpr = CprSig (CprType arty cpr)

seqCprSig :: CprSig -> ()
seqCprSig (CprSig ty) = seqCprTy ty

-- | BNF:
-- ```
--   cpr ::= ''                               -- TopCpr
--        |  n                                -- FlatConCpr n
--        |  n '(' cpr1 ',' cpr2 ',' ... ')'  -- ConCpr n [cpr1,cpr2,...]
--        |  'b'                              -- BotCpr
-- ```
-- Examples:
--   * `f x = f x` has denotation `b`
--   * `1(1,)` is a valid (nested) 'Cpr' denotation for `(I# 42#, f 42)`.
instance Outputable Cpr where
  ppr TopCpr         = empty
  ppr (FlatConCpr n) = int n
  ppr (ConCpr n cs)  = int n <> parens (pprWithCommas ppr cs)
  ppr BotCpr         = char 'b'

instance Outputable CprType where
  ppr (CprType arty res) = ppr arty <> ppr res

-- | Only print the CPR result
instance Outputable CprSig where
  ppr (CprSig ty) = ppr (ct_cpr ty)

instance Binary Cpr where
  put_ bh TopCpr         = putByte bh 0
  put_ bh BotCpr         = putByte bh 1
  put_ bh (FlatConCpr n) = putByte bh 2 *> put_ bh n
  put_ bh (ConCpr n cs)  = putByte bh 3 *> put_ bh n *> put_ bh cs
  get  bh = do
    h <- getByte bh
    case h of
      0 -> return TopCpr
      1 -> return BotCpr
      2 -> FlatConCpr <$> get bh
      3 -> ConCpr <$> get bh <*> get bh
      _ -> pprPanic "Binary Cpr: Invalid tag" (int (fromIntegral h))

instance Binary CprType where
  put_ bh (CprType arty cpr) = put_ bh arty *> put_ bh cpr
  get  bh                    = CprType <$> get bh <*> get bh
