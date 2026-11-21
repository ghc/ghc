{-# LANGUAGE ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeLetQuote (result) where

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

-- A let can return a type constructor. The final argument applies to its result.
applied :: $(do
  f <- newName "f"
  appT (letT f (conT ''Maybe) (varT f)) (conT ''Char))
applied = Just 'q'

$([d| type QuotedPair a = let a = Maybe a in (a, a) |])

result :: (Either (Maybe Bool) (Maybe Bool), Maybe Char, QuotedPair Int)
result = (quoted, applied, (Just 3, Nothing))
