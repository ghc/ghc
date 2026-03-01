{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Type.Reflection
import GHC.Hs

main :: IO ()
main = do
  checkAllExtFields
  checkAllDirectFields

checkAllExtFields :: IO ()
checkAllExtFields = do
  -- No corresponding HsExpr: HsListTy HsTupleTy HsSumTy HsIParamTy HsDocTy

  putStrLn "Extension fields @GhcPs\n-----------------------"
  checkExtField "Var"      (HsVar @GhcPs) (HsTyVar @GhcPs)
  checkExtField "LitE"     (HsLit @GhcPs) (HsTyLit @GhcPs)
  checkExtField "Par"      (HsPar @GhcPs) (HsParTy @GhcPs)
  checkExtField "App"      (HsApp @GhcPs) (HsAppTy @GhcPs)
  checkExtField "AppTypeE" (HsAppType @GhcPs) (HsAppKindTy @GhcPs)
  checkExtField "OpApp"    (OpApp @GhcPs) (HsOpTy @GhcPs)
  checkExtField "ForAll"   (HsForAll @GhcPs) (HsForAllTy @GhcPs)
  checkExtField "Qual"     (HsQual @GhcPs) (HsQualTy @GhcPs)
  checkExtField "Star"     (HsStar @GhcPs) (HsStarTy @GhcPs)
  checkExtField "FunArr"   (HsFunArr @GhcPs) (HsFunTy @GhcPs)
  checkExtField "ExprWithTySig" (ExprWithTySig @GhcPs) (HsKindSig @GhcPs)
  checkExtField "UntypedSplice" (HsUntypedSplice @GhcPs) (HsSpliceTy @GhcPs)
  checkExtField "ExplicitList"  (ExplicitList @GhcPs) (HsExplicitListTy @GhcPs)
  checkExtField "ExplicitTuple" (ExplicitTuple @GhcPs) (HsExplicitTupleTy @GhcPs)
  checkExtField "Hole"     (HsHole @GhcPs) (HsWildCardTy @GhcPs)

  putStrLn "\nExtension fields @GhcRn\n-----------------------"
  checkExtField "Var"      (HsVar @GhcRn) (HsTyVar @GhcRn)
  checkExtField "LitE"     (HsLit @GhcRn) (HsTyLit @GhcRn)
  checkExtField "Par"      (HsPar @GhcRn) (HsParTy @GhcRn)
  checkExtField "App"      (HsApp @GhcRn) (HsAppTy @GhcRn)
  checkExtField "AppTypeE" (HsAppType @GhcRn) (HsAppKindTy @GhcRn)
  checkExtField "OpApp"    (OpApp @GhcRn) (HsOpTy @GhcRn)
  checkExtField "ForAll"   (HsForAll @GhcRn) (HsForAllTy @GhcRn)
  checkExtField "Qual"     (HsQual @GhcRn) (HsQualTy @GhcRn)
  checkExtField "Star"     (HsStar @GhcRn) (HsStarTy @GhcRn)
  checkExtField "FunArr"   (HsFunArr @GhcRn) (HsFunTy @GhcRn)
  checkExtField "ExprWithTySig" (ExprWithTySig @GhcRn) (HsKindSig @GhcRn)
  checkExtField "UntypedSplice" (HsUntypedSplice @GhcRn) (HsSpliceTy @GhcRn)
  checkExtField "ExplicitList"  (ExplicitList @GhcRn) (HsExplicitListTy @GhcRn)
  checkExtField "ExplicitTuple" (ExplicitTuple @GhcRn) (HsExplicitTupleTy @GhcRn)
  checkExtField "Hole"     (HsHole @GhcRn) (HsWildCardTy @GhcRn)

  putStrLn "\nExtension fields @GhcTc\n-----------------------"
  checkExtField "Var"      (HsVar @GhcTc) (HsTyVar @GhcTc)
  checkExtField "LitE"     (HsLit @GhcTc) (HsTyLit @GhcTc)
  checkExtField "Par"      (HsPar @GhcTc) (HsParTy @GhcTc)
  checkExtField "App"      (HsApp @GhcTc) (HsAppTy @GhcTc)
  checkExtField "AppTypeE" (HsAppType @GhcTc) (HsAppKindTy @GhcTc)
  checkExtField "OpApp"    (OpApp @GhcTc) (HsOpTy @GhcTc)
  checkExtField "ForAll"   (HsForAll @GhcTc) (HsForAllTy @GhcTc)
  checkExtField "Qual"     (HsQual @GhcTc) (HsQualTy @GhcTc)
  checkExtField "Star"     (HsStar @GhcTc) (HsStarTy @GhcTc)
  checkExtField "FunArr"   (HsFunArr @GhcTc) (HsFunTy @GhcTc)
  checkExtField "ExprWithTySig" (ExprWithTySig @GhcTc) (HsKindSig @GhcTc)
  checkExtField "UntypedSplice" (HsUntypedSplice @GhcTc) (HsSpliceTy @GhcTc)
  checkExtField "ExplicitList"  (ExplicitList @GhcTc) (HsExplicitListTy @GhcTc)
  checkExtField "ExplicitTuple" (ExplicitTuple @GhcTc) (HsExplicitTupleTy @GhcTc)
  checkExtField "Hole"     (HsHole @GhcTc) (HsWildCardTy @GhcTc)

checkAllDirectFields :: IO ()
checkAllDirectFields = do
  -- No corresponding HsExpr: HsListTy HsTupleTy HsSumTy HsIParamTy HsDocTy
  putStrLn "\nDirect fields\n-------------"
  checkDirectFields "Var"      (HsVar @GhcPs) (HsTyVar @GhcPs)
  checkDirectFields "LitE"     (HsLit @GhcPs) (HsTyLit @GhcPs)
  checkDirectFields "Par"      (HsPar @GhcPs) (HsParTy @GhcPs)
  checkDirectFields "App"      (HsApp @GhcPs) (HsAppTy @GhcPs)
  checkDirectFields "AppTypeE" (HsAppType @GhcPs) (HsAppKindTy @GhcPs)
  checkDirectFields "OpApp"    (OpApp @GhcPs) (HsOpTy @GhcPs)
  checkDirectFields "ForAll"   (HsForAll @GhcPs) (HsForAllTy @GhcPs)
  checkDirectFields "Qual"     (HsQual @GhcPs) (HsQualTy @GhcPs)
  checkDirectFields "Star"     (HsStar @GhcPs) (HsStarTy @GhcPs)
  checkDirectFields "FunArr"   (HsFunArr @GhcPs) (HsFunTy @GhcPs)
  checkDirectFields "ExprWithTySig" (ExprWithTySig @GhcPs) (HsKindSig @GhcPs)
  checkDirectFields "UntypedSplice" (HsUntypedSplice @GhcPs) (HsSpliceTy @GhcPs)
  checkDirectFields "ExplicitList"  (ExplicitList @GhcPs) (HsExplicitListTy @GhcPs)
  checkDirectFields "ExplicitTuple" (ExplicitTuple @GhcPs) (HsExplicitTupleTy @GhcPs)
  checkDirectFields "Hole"     (HsHole @GhcPs) (HsWildCardTy @GhcPs)

data P   -- placeholder for the pass

type Replace :: k -> k
type family Replace t where
  Replace (GhcPass _) = P
  Replace HsType = HsExpr
  Replace (f a) = Replace f (Replace a)
  Replace t = t

checkDirectFields :: forall r1 r2. String ->
  forall x1 x2. forall (con1 :: x1 -> r1) (con2 :: x2 -> r2) ->
  (Typeable (Replace r1), Typeable (Replace r2)) => IO ()
checkDirectFields @r1 @r2 ctx _ _ =
  let aRep = typeRep @(Replace r1)
      bRep = typeRep @(Replace r2)
  in case eqTypeRep aRep bRep of
       Nothing -> do
         putStrLn $ "T(" ++ ctx ++ ") mismatch"
         putStrLn $ "  >>> " ++ show aRep
         putStrLn $ "  <<< " ++ show bRep
       Just HRefl -> do
         putStrLn $ "T(" ++ ctx ++ ") match = " ++ show aRep

checkExtField :: forall x1 x2. String ->
  forall r1 r2. forall (con1 :: x1 -> r1) (con2 :: x2 -> r2) ->
  (Typeable x1, Typeable x2) => IO ()
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
