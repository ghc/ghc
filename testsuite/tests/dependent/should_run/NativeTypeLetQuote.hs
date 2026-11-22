{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, TemplateHaskell #-}
module NativeTypeLetQuote (result) where

import Data.Proxy (Proxy(..))
import Language.Haskell.TH

-- The first RHS in the group uses the enclosing binder. The second RHS
-- uses the first group binder. Both fields use the second group binder.
quoted :: $(do
  ty <- [t| let a = Bool in let { wrapped = Maybe a; a = wrapped } in Either a a |]
  case ty of
    LetT outer (ConT bool)
      (LetT first (AppT (ConT maybeTy) (VarT outerUse))
        (LetT second (VarT firstUse)
          (AppT (AppT (ConT eitherTy) (VarT left)) (VarT right))))
      | outer == outerUse, first == firstUse, second == left, second == right
      , outer /= second
      , bool == ''Bool, maybeTy == ''Maybe, eitherTy == ''Either -> pure ty
    _ -> fail (show ty))
quoted = Right (Just True)

-- The signature uses the first tag. The let body uses the second tag.
signed :: $(do
  ty <- [t| let { tag = Bool; tag :: tag; tag = 'True } in Proxy tag |]
  case ty of
    LetT outer (ConT bool)
      (LetT inner (SigT (PromotedT true) (VarT kind)) (AppT (ConT proxy) (VarT use)))
      | outer == kind, inner == use, outer /= inner
      , bool == ''Bool, true == 'True, proxy == ''Proxy -> pure ty
    _ -> fail (show ty))
signed = Proxy :: Proxy 'True

pair :: $(do
  ty <- [t| let pair = '(1, 7) in let (rows, cols) = '(2, 3) in Proxy '(pair, '(rows, cols)) |]
  case ty of
    LetT outer _ (LetT saved _ (LetT rows (AppT (ConT first) (VarT left))
      (LetT cols (AppT (ConT second) (VarT right)) _)))
      | saved == left, saved == right, rows /= cols, outer /= saved
      , nameBase outer == nameBase saved
      , nameBase first == "PairFirst", nameBase second == "PairSecond" -> pure ty
    _ -> fail (show ty))
pair = Proxy :: Proxy '( '(1, 7), '(2, 3))

nestedPair :: $(do
  ty <- [t| let width = 7 in
      let ((width, _), height) = '( '(width, 'True), 3)
      in Proxy '(width, height) |]
  case ty of
    LetT outer _ (LetT saved _ (LetT child (AppT (ConT first) (VarT parentUse))
      (LetT width (AppT (ConT childFirst) (VarT childUse))
        (LetT _ (AppT (ConT second) (VarT siblingUse)) _))))
      | saved == parentUse, saved == siblingUse, child == childUse, saved /= child
      , outer /= width, nameBase outer == nameBase width
      , nameBase first == "PairFirst", first == childFirst
      , nameBase second == "PairSecond" -> pure ty
    _ -> fail (show ty))
nestedPair = Proxy :: Proxy '(7, 3)

-- A let can return a type constructor. The final argument applies to its result.
applied :: $(do
  f <- newName "f"
  appT (letT f (conT ''Maybe) (varT f)) (conT ''Char))
applied = Just 'q'

$([d| type QuotedPair a = let a = Maybe a in (a, a) |])

$([d| type QuotedCase (b :: Bool) =
        let result :: Maybe Bool
            result = case b of
              'True -> 'Nothing
              'False -> 'Nothing
        in Proxy result |])

quotedCase :: QuotedCase 'True
quotedCase = Proxy :: Proxy ('Nothing :: Maybe Bool)

result :: (Either (Maybe Bool) (Maybe Bool), Maybe Char, QuotedPair Int)
result = quotedCase `seq` nestedPair `seq` pair `seq` signed `seq` (quoted, applied, (Just 3, Nothing))
