{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind
import Data.Type.Equality
import Type.Reflection
import GHC.TypeLits
import GHC.Hs as T (type GhcPs)
import GHC.Hs

main :: IO ()
main = do
  checkAllExtFields
  checkAllDirectFields

checkAllExtFields :: IO ()
checkAllExtFields = do
  -- No corresponding HsExpr: HsListTy HsTupleTy HsSumTy HsIParamTy HsDocTy

  putStrLn "Extension fields @GhcPs\n-----------------------"
  diffExtField "Var"      (HsVar @GhcPs) (HsTyVar @GhcPs)
  eqExtField   "LitE"     (HsLit @GhcPs) (HsTyLit @GhcPs)
  eqExtField   "Par"      (HsPar @GhcPs) (HsParTy @GhcPs)
  eqExtField   "App"      (HsApp @GhcPs) (HsAppTy @GhcPs)
  eqExtField   "AppTypeE" (HsAppType @GhcPs) (HsAppKindTy @GhcPs)
  eqExtField   "OpApp"    (OpApp @GhcPs) (HsOpTy @GhcPs)
  eqExtField   "ForAll"   (HsForAll @GhcPs) (HsForAllTy @GhcPs)
  eqExtField   "Qual"     (HsQual @GhcPs) (HsQualTy @GhcPs)
  eqExtField   "Star"     (HsStar @GhcPs) (HsStarTy @GhcPs)
  eqExtField   "FunArr"   (HsFunArr @GhcPs) (HsFunTy @GhcPs)
  eqExtField   "ExprWithTySig" (ExprWithTySig @GhcPs) (HsKindSig @GhcPs)
  eqExtField   "UntypedSplice" (HsUntypedSplice @GhcPs) (HsSpliceTy @GhcPs)
  diffExtField "ExplicitList"  (ExplicitList @GhcPs) (HsExplicitListTy @GhcPs)
  diffExtField "ExplicitTuple" (ExplicitTuple @GhcPs) (HsExplicitTupleTy @GhcPs)
  diffExtField "Hole"     (HsHole @GhcPs) (HsWildCardTy @GhcPs)

  putStrLn "\nExtension fields @GhcRn\n-----------------------"
  diffExtField "Var"      (HsVar @GhcRn) (HsTyVar @GhcRn)
  eqExtField   "LitE"     (HsLit @GhcRn) (HsTyLit @GhcRn)
  diffExtField "Par"      (HsPar @GhcRn) (HsParTy @GhcRn)
  eqExtField   "App"      (HsApp @GhcRn) (HsAppTy @GhcRn)
  eqExtField   "AppTypeE" (HsAppType @GhcRn) (HsAppKindTy @GhcRn)
  diffExtField "OpApp"    (OpApp @GhcRn) (HsOpTy @GhcRn)
  eqExtField   "ForAll"   (HsForAll @GhcRn) (HsForAllTy @GhcRn)
  eqExtField   "Qual"     (HsQual @GhcRn) (HsQualTy @GhcRn)
  eqExtField   "Star"     (HsStar @GhcRn) (HsStarTy @GhcRn)
  eqExtField   "FunArr"   (HsFunArr @GhcRn) (HsFunTy @GhcRn)
  diffExtField "ExprWithTySig" (ExprWithTySig @GhcRn) (HsKindSig @GhcRn)
  diffExtField "UntypedSplice" (HsUntypedSplice @GhcRn) (HsSpliceTy @GhcRn)
  eqExtField   "ExplicitList"  (ExplicitList @GhcRn) (HsExplicitListTy @GhcRn)
  eqExtField   "ExplicitTuple" (ExplicitTuple @GhcRn) (HsExplicitTupleTy @GhcRn)
  diffExtField "Hole"     (HsHole @GhcRn) (HsWildCardTy @GhcRn)

  putStrLn "\nExtension fields @GhcTc\n-----------------------"
  diffExtField "Var"      (HsVar @GhcTc) (HsTyVar @GhcTc)
  eqExtField   "LitE"     (HsLit @GhcTc) (HsTyLit @GhcTc)
  diffExtField "Par"      (HsPar @GhcTc) (HsParTy @GhcTc)
  eqExtField   "App"      (HsApp @GhcTc) (HsAppTy @GhcTc)
  diffExtField "AppTypeE" (HsAppType @GhcTc) (HsAppKindTy @GhcTc)
  diffExtField "OpApp"    (OpApp @GhcTc) (HsOpTy @GhcTc)
  diffExtField "ForAll"   (HsForAll @GhcTc) (HsForAllTy @GhcTc)
  diffExtField "Qual"     (HsQual @GhcTc) (HsQualTy @GhcTc)
  diffExtField "Star"     (HsStar @GhcTc) (HsStarTy @GhcTc)
  diffExtField "FunArr"   (HsFunArr @GhcTc) (HsFunTy @GhcTc)
  diffExtField "ExprWithTySig" (ExprWithTySig @GhcTc) (HsKindSig @GhcTc)
  diffExtField "UntypedSplice" (HsUntypedSplice @GhcTc) (HsSpliceTy @GhcTc)
  eqExtField   "ExplicitList"  (ExplicitList @GhcTc) (HsExplicitListTy @GhcTc)
  diffExtField "ExplicitTuple" (ExplicitTuple @GhcTc) (HsExplicitTupleTy @GhcTc)
  diffExtField "Hole"     (HsHole @GhcTc) (HsWildCardTy @GhcTc)

checkAllDirectFields :: IO ()
checkAllDirectFields = do
  -- No corresponding HsExpr: HsListTy HsTupleTy HsSumTy HsIParamTy HsDocTy
  putStrLn "\nDirect fields\n-------------"
  diffDirectFields "Var"      (HsVar @GhcPs) (HsTyVar @GhcPs)
  eqDirectFields   "LitE"     (HsLit @GhcPs) (HsTyLit @GhcPs)
  eqDirectFields   "Par"      (HsPar @GhcPs) (HsParTy @GhcPs)
  eqDirectFields   "App"      (HsApp @GhcPs) (HsAppTy @GhcPs)
  diffDirectFields "AppTypeE" (HsAppType @GhcPs) (HsAppKindTy @GhcPs)
  diffDirectFields "OpApp"    (OpApp @GhcPs) (HsOpTy @GhcPs)
  eqDirectFields   "ForAll"   (HsForAll @GhcPs) (HsForAllTy @GhcPs)
  eqDirectFields   "Qual"     (HsQual @GhcPs) (HsQualTy @GhcPs)
  eqDirectFields   "Star"     (HsStar @GhcPs) (HsStarTy @GhcPs)
  eqDirectFields   "FunArr"   (HsFunArr @GhcPs) (HsFunTy @GhcPs)
  diffDirectFields "ExprWithTySig" (ExprWithTySig @GhcPs) (HsKindSig @GhcPs)
  eqDirectFields   "UntypedSplice" (HsUntypedSplice @GhcPs) (HsSpliceTy @GhcPs)
  diffDirectFields "ExplicitList"  (ExplicitList @GhcPs) (HsExplicitListTy @GhcPs)
  diffDirectFields "ExplicitTuple" (ExplicitTuple @GhcPs) (HsExplicitTupleTy @GhcPs)
  eqDirectFields   "Hole"     (HsHole @GhcPs) (HsWildCardTy @GhcPs)

-- Extension fields are equal. Hooray!
eqExtField :: (x1 ~ x2) => ExtFieldChecker x1 x2
eqExtField ctx con1 con2 = checkExtField ctx con1 con2

-- Extension fields currently differ.
-- They need to be unified before we can declare `type HsType = HsExpr`.
diffExtField :: ((x1 == x2) ~ False) => ExtFieldChecker x1 x2
diffExtField ctx con1 con2 = checkExtField ctx con1 con2

-- Direct fields are equal. Hooray!
eqDirectFields :: (Replace r1 ~ Replace r2) => DirectFieldsChecker r1 r2
eqDirectFields ctx con1 con2 = checkDirectFields ctx con1 con2

-- Extension fields currently differ.
-- They need to be unified before we can declare `type HsType = HsExpr`.
diffDirectFields :: ((Replace r1 == Replace r2) ~ False) => DirectFieldsChecker r1 r2
diffDirectFields ctx con1 con2 = checkDirectFields ctx con1 con2

data P   -- placeholder for the pass

type Replace :: k -> k
type family Replace t where
  Replace (GhcPass _) = P
  Replace HsType = HsExpr
  Replace (f a) = Replace f (Replace a)
  Replace t = t

type DirectFieldsChecker r1 r2 =
  String ->
    forall x1 x2. forall (con1 :: x1 -> r1) (con2 :: x2 -> r2) ->
    (Typeable (Replace r1), Typeable (Replace r2)) =>
    IO ()

checkDirectFields :: forall r1 r2. DirectFieldsChecker r1 r2
checkDirectFields @r1 @r2 ctx _ _ =
  let aRep = typeRep @(Replace r1)
      bRep = typeRep @(Replace r2)
  in case eqTypeRep aRep bRep of
       Nothing -> do
         putStrLn $ "Fields(" ++ ctx ++ ") mismatch"
         putStrLn $ "  >>> " ++ show aRep
         putStrLn $ "  <<< " ++ show bRep
       Just HRefl -> do
         putStrLn $ "Fields(" ++ ctx ++ ") match = " ++ show aRep

type ExtFieldChecker x1 x2 = String -> forall r1 r2. forall (con1 :: x1 -> r1) (con2 :: x2 -> r2) -> (Typeable x1, Typeable x2) => IO ()

checkExtField :: forall x1 x2. ExtFieldChecker x1 x2
checkExtField @x1 @x2 ctx _ _ =
  let aRep = typeRep @x1
      bRep = typeRep @x2
  in case eqTypeRep aRep bRep of
       Nothing    -> do
         putStrLn $ "X(" ++ ctx ++ ") mismatch"
         putStrLn $ "  >>> " ++ show aRep
         putStrLn $ "  <<< " ++ show bRep
       Just HRefl -> do
         putStrLn $ "X(" ++ ctx ++ ") match = " ++ show aRep
