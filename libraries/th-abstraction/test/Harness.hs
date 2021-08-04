{-# Language CPP, TemplateHaskell #-}

{-|
Module      : Harness
Description : Comparison functions for data type info used in tests
Copyright   : Eric Mertens 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides comparison functions that are able to check
that the computed 'DatatypeInfo' values match the expected ones
up to alpha renaming.

-}
module Harness
  ( validateDI
  , validateCI
  , equateCxt

    -- * Utilities
  , varKCompat
  ) where

import           Control.Monad
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib (starK)

validateDI :: DatatypeInfo -> DatatypeInfo -> ExpQ
validateDI = validate equateDI

validateCI :: ConstructorInfo -> ConstructorInfo -> ExpQ
validateCI = validate equateCI

validate :: (a -> a -> Either String ()) -> a -> a -> ExpQ
validate equate x y = either fail (\_ -> [| return () |]) (equate x y)

-- | If the arguments are equal up to renaming return @'Right' ()@,
-- otherwise return a string exlaining the mismatch.
equateDI :: DatatypeInfo -> DatatypeInfo -> Either String ()
equateDI dat1 dat2 =
  do check "datatypeName"          (nameBase . datatypeName)    dat1 dat2
     check "datatypeVars len"      (length . datatypeVars)      dat1 dat2
     check "datatypeInstTypes len" (length . datatypeInstTypes) dat1 dat2
     check "datatypeVariant"       datatypeVariant              dat1 dat2
     check "datatypeCons len"      (length . datatypeCons)      dat1 dat2

     let sub = Map.fromList (zip (freeVariables (bndrParams (datatypeVars dat2)))
                                 (map VarT (freeVariables (bndrParams (datatypeVars dat1)))))

     check "datatypeVars" id
       (datatypeVars dat1)
       (substIntoTyVarBndrs sub (datatypeVars dat2))

     check "datatypeInstTypes" id
       (datatypeInstTypes dat1)
       (applySubstitution sub (datatypeInstTypes dat2))

     zipWithM_ (equateCxt "datatypeContext")
       (datatypeContext dat1)
       (applySubstitution sub (datatypeContext dat2))

     zipWithM_ equateCI
       (datatypeCons dat1)
       (datatypeCons dat2) -- Don't bother applying the substitution here, as
                           -- equateCI takes care of this for us

equateCxt :: String -> Pred -> Pred -> Either String ()
equateCxt lbl pred1 pred2 =
  do check (lbl ++ " class")    asClassPred pred1 pred2
     check (lbl ++ " equality") asEqualPred pred1 pred2

-- | If the arguments are equal up to renaming return @'Right' ()@,
-- otherwise return a string exlaining the mismatch.
equateCI :: ConstructorInfo -> ConstructorInfo -> Either String ()
equateCI con1 con2 =
  do check "constructorName"       (nameBase . constructorName) con1 con2
     check "constructorVariant"    constructorVariantBase       con1 con2

     let sub1 = Map.fromList (zip (freeVariables (bndrParams (constructorVars con2)))
                                  (map VarT (freeVariables (bndrParams (constructorVars con1)))))
         sub2 = Map.fromList (zip (freeVariables con2)
                                  (map VarT (freeVariables con1)))
         sub  = Map.unions [sub1, sub2]

     zipWithM_ (equateCxt "constructorContext")
        (constructorContext con1)
        (applySubstitution sub (constructorContext con2))

     check "constructorVars" id
        (constructorVars con1)
        (substIntoTyVarBndrs sub (constructorVars con2))

     check "constructorFields" id
        (constructorFields con1)
        (applySubstitution sub (constructorFields con2))

     zipWithM_ equateStrictness
        (constructorStrictness con1)
        (constructorStrictness con2)
  where
    constructorVariantBase :: ConstructorInfo -> ConstructorVariant
    constructorVariantBase con =
      case constructorVariant con of
        NormalConstructor        -> NormalConstructor
        i@InfixConstructor{}     -> i
        RecordConstructor fields -> RecordConstructor $ map (mkName . nameBase) fields

-- Substitutes both type variable names and kinds.
substIntoTyVarBndrs :: Map Name Type -> [TyVarBndr_ flag] -> [TyVarBndr_ flag]
substIntoTyVarBndrs subst = map go
  where
    go = mapTV (substName subst) id (applySubstitution subst)

    substName :: Map Name Type -> Name -> Name
    substName subst n = fromMaybe n $ do
      nty <- Map.lookup n subst
      case nty of
        VarT n' -> Just n'
        _       -> Nothing

bndrParams :: [TyVarBndr_ flag] -> [Type]
bndrParams = map $ elimTV VarT (\n k -> SigT (VarT n) k)

equateStrictness :: FieldStrictness -> FieldStrictness -> Either String ()
equateStrictness fs1 fs2 =
  check "constructorStrictness" oldGhcHack fs1 fs2
  where
#if MIN_VERSION_template_haskell(2,7,0)
    oldGhcHack = id
#else
    -- GHC 7.0 and 7.2 didn't have an Unpacked TH constructor, so as a
    -- simple workaround, we will treat unpackedAnnot as isStrictAnnot
    -- (the closest equivalent).
    oldGhcHack fs
      | fs == unpackedAnnot = isStrictAnnot
      | otherwise           = fs
#endif

check :: (Show b, Eq b) => String -> (a -> b) -> a -> a -> Either String ()
check lbl f x y
  | f x == f y = Right ()
  | otherwise  = Left (lbl ++ ":\n\n" ++ show (f x) ++ "\n\n" ++ show (f y))

-- If on a recent-enough version of Template Haskell, construct a kind variable.
-- Otherwise, default to starK.
varKCompat :: Name -> Kind
#if MIN_VERSION_template_haskell(2,8,0)
varKCompat = VarT
#else
varKCompat _ = starK
#endif
