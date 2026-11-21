{-# LANGUAGE ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeLetQuote (result) where

import Language.Haskell.TH

-- The RHS of the second let uses the first binder. Both fields use the second.
quoted :: $(do
  ty <- [t| let a = Bool in let a = Maybe a in Either a a |]
  case ty of
    LetT outer (ConT bool)
      (LetT inner (AppT (ConT maybeTy) (VarT outerUse))
        (AppT (AppT (ConT eitherTy) (VarT left)) (VarT right)))
      | outer == outerUse, inner == left, inner == right, outer /= inner
      , bool == ''Bool, maybeTy == ''Maybe, eitherTy == ''Either -> pure ty
    _ -> fail (show ty))
quoted = Right (Just True)

-- A let can return a type constructor. The final argument applies to its result.
applied :: $(do
  f <- newName "f"
  appT (letT f (conT ''Maybe) (varT f)) (conT ''Char))
applied = Just 'q'

$([d| type QuotedPair a = let a = Maybe a in (a, a) |])

result :: (Either (Maybe Bool) (Maybe Bool), Maybe Char, QuotedPair Int)
result = (quoted, applied, (Just 3, Nothing))
