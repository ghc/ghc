{-# Language CPP, FlexibleContexts, TypeFamilies, KindSignatures, TemplateHaskell, GADTs #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 807
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
#endif

#if MIN_VERSION_template_haskell(2,8,0)
{-# Language PolyKinds #-}
#endif

{-|
Module      : Main
Description : Test cases for the th-abstraction package
Copyright   : Eric Mertens 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module checks that the 'reifyDatatype' logic works consistently
across a wide range of datatypes. These tests are validated across
the versions of GHC supported by this package.

-}
module Main (main) where

#if __GLASGOW_HASKELL__ >= 704
import           Control.Monad (zipWithM_)
#endif

import           Control.Monad (unless)
import qualified Data.Map as Map

#if MIN_VERSION_base(4,7,0)
import           Data.Type.Equality ((:~:)(..))
#endif

import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib (starK)

import           Harness
import           Types

-- | Test entry point. Tests will pass or fail at compile time.
main :: IO ()
main =
  do adt1Test
     gadt1Test
     gadt2Test
     gadtrec1Test
     equalTest
     showableTest
     recordTest
     voidstosTest
     strictDemoTest
     recordVanillaTest
#if MIN_VERSION_template_haskell(2,6,0)
     t43Test
     t58Test
#endif
#if MIN_VERSION_template_haskell(2,7,0)
     dataFamilyTest
     ghc78bugTest
     quotedTest
     polyTest
     gadtFamTest
     famLocalDecTest1
     famLocalDecTest2
     recordFamTest
     t46Test
     t73Test
#endif
     fixityLookupTest
#if __GLASGOW_HASKELL__ >= 704
     resolvePredSynonymsTest
#endif
     reifyDatatypeWithConNameTest
     reifyConstructorTest
#if MIN_VERSION_base(4,7,0)
     importedEqualityTest
#endif
#if MIN_VERSION_template_haskell(2,8,0)
     kindSubstTest
     t59Test
     t61Test
     t66Test
     t80Test
#endif
#if MIN_VERSION_template_haskell(2,11,0)
     t79Test
#endif
#if __GLASGOW_HASKELL__ >= 800
     t37Test
     polyKindedExTyvarTest
#endif
#if __GLASGOW_HASKELL__ >= 807
     resolveTypeSynonymsVKATest
#endif
     regressionTest44
     t63Test
     t70Test

adt1Test :: IO ()
adt1Test =
  $(do info <- reifyDatatype ''Adt1

       let names            = map mkName ["a","b"]
           [aTvb,bTvb]      = map (\v -> kindedTV v starK) names
           vars@[aVar,bVar] = map (VarT . mkName) ["a","b"]
           [aSig,bSig]      = map (\v -> SigT v starK) vars

       validateDI info
         DatatypeInfo
           { datatypeName = ''Adt1
           , datatypeContext = []
           , datatypeVars = [aTvb,bTvb]
           , datatypeInstTypes = [aSig, bSig]
           , datatypeVariant = Datatype
           , datatypeCons =
               [ ConstructorInfo
                   { constructorName = 'Adtc1
                   , constructorContext = []
                   , constructorVars = []
                   , constructorFields = [AppT (AppT (TupleT 2) aVar) bVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = 'Adtc2
                   , constructorContext = []
                   , constructorVars = []
                   , constructorFields = [ConT ''Bool, ConT ''Int]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant = InfixConstructor }
               ]
           }
   )

gadt1Test :: IO ()
gadt1Test =
  $(do info <- reifyDatatype ''Gadt1

       let a = mkName "a"
           aVar = VarT a

       validateDI info
         DatatypeInfo
           { datatypeName = ''Gadt1
           , datatypeContext = []
           , datatypeVars = [kindedTV a starK]
           , datatypeInstTypes = [SigT aVar starK]
           , datatypeVariant = Datatype
           , datatypeCons =
               [ ConstructorInfo
                   { constructorName = 'Gadtc1
                   , constructorVars = []
                   , constructorContext = [equalPred aVar (ConT ''Int)]
                   , constructorFields = [ConT ''Int]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = 'Gadtc2
                   , constructorVars = []
                   , constructorContext = []
                   , constructorFields = [AppT (AppT (TupleT 2) aVar) aVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = '(:**:)
                   , constructorVars = []
                   , constructorContext = [equalPred aVar (TupleT 0)]
                   , constructorFields = [ConT ''Bool, ConT ''Char]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant = InfixConstructor }
               , ConstructorInfo
                   { constructorName = '(:!!:)
                   , constructorVars = []
                   , constructorContext = [equalPred aVar (ConT ''Double)]
                   , constructorFields = [ConT ''Char, ConT ''Bool]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               ]
           }
   )

gadtrec1Test :: IO ()
gadtrec1Test =
  $(do info <- reifyDatatype ''Gadtrec1

       let a   = mkName "a"
           con = gadtRecVanillaCI

       validateDI info
         DatatypeInfo
           { datatypeName      = ''Gadtrec1
           , datatypeContext   = []
           , datatypeVars      = [kindedTV a starK]
           , datatypeInstTypes = [SigT (VarT a) starK]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ con, con { constructorName = 'Gadtrecc2 } ]
           }
   )

equalTest :: IO ()
equalTest =
  $(do info <- reifyDatatype ''Equal

       let names                 = map mkName ["a","b","c"]
           [aTvb,bTvb,cTvb]      = map (\v -> kindedTV v starK) names
           vars@[aVar,bVar,cVar] = map VarT names
           [aSig,bSig,cSig]      = map (\v -> SigT v starK) vars

       validateDI info
         DatatypeInfo
           { datatypeName      = ''Equal
           , datatypeContext   = []
           , datatypeVars      = [aTvb, bTvb, cTvb]
           , datatypeInstTypes = [aSig, bSig, cSig]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'Equalc
                   , constructorVars       = []
                   , constructorContext    =
                        [ equalPred aVar cVar
                        , equalPred bVar cVar
                        , classPred ''Read [cVar]
                        , classPred ''Show [cVar]
                        ]
                   , constructorFields     =
                        [ListT `AppT` cVar, ConT ''Maybe `AppT` cVar]
                   , constructorStrictness =
                        [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               ]
           }
   )

showableTest :: IO ()
showableTest =
  $(do info <- reifyDatatype ''Showable

       let a = mkName "a"

       validateDI info
         DatatypeInfo
           { datatypeName      = ''Showable
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'Showable
                   , constructorVars       = [kindedTV a starK]
                   , constructorContext    = [classPred ''Show [VarT a]]
                   , constructorFields     = [VarT a]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               ]
           }
   )

recordTest :: IO ()
recordTest =
  $(do info <- reifyDatatype ''R
       validateDI info
         DatatypeInfo
           { datatypeName      = ''R
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'R1
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Int, ConT ''Int]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = RecordConstructor ['field1, 'field2] }
               ]
           }
   )

gadt2Test :: IO ()
gadt2Test =
  $(do info <- reifyDatatype ''Gadt2
       let names            = map mkName ["a","b"]
           [aTvb,bTvb]      = map (\v -> kindedTV v starK) names
           vars@[aVar,bVar] = map VarT names
           [aSig,bSig]      = map (\v -> SigT v starK) vars
           x                = mkName "x"

           con = ConstructorInfo
                     { constructorName       = undefined
                     , constructorVars       = []
                     , constructorContext    = []
                     , constructorFields     = []
                     , constructorStrictness = []
                     , constructorVariant    = NormalConstructor }
       validateDI info
         DatatypeInfo
           { datatypeName      = ''Gadt2
           , datatypeContext   = []
           , datatypeVars      = [aTvb, bTvb]
           , datatypeInstTypes = [aSig, bSig]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ con { constructorName = 'Gadt2c1
                     , constructorContext = [equalPred bVar (AppT ListT aVar)] }
               , con { constructorName = 'Gadt2c2
                     , constructorContext = [equalPred aVar (AppT ListT bVar)] }
               , con { constructorName = 'Gadt2c3
                     , constructorVars = [kindedTV x starK]
                     , constructorContext =
                         [equalPred aVar (AppT ListT (VarT x))
                         ,equalPred bVar (AppT ListT (VarT x))] } ]
           }
  )

voidstosTest :: IO ()
voidstosTest =
  $(do info <- reifyDatatype ''VoidStoS
       let g = mkName "g"
       validateDI info
         DatatypeInfo
           { datatypeName      = ''VoidStoS
           , datatypeContext   = []
           , datatypeVars      = [kindedTV g (arrowKCompat starK starK)]
           , datatypeInstTypes = [SigT (VarT g) (arrowKCompat starK starK)]
           , datatypeVariant   = Datatype
           , datatypeCons      = []
           }
  )

strictDemoTest :: IO ()
strictDemoTest =
  $(do info <- reifyDatatype ''StrictDemo
       validateDI info
         DatatypeInfo
           { datatypeName      = ''StrictDemo
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'StrictDemo
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Int, ConT ''Int, ConT ''Int]
                   , constructorStrictness = [ notStrictAnnot
                                             , isStrictAnnot
                                             , unpackedAnnot
                                             ]
                   , constructorVariant    = NormalConstructor } ]
           }
   )

recordVanillaTest :: IO ()
recordVanillaTest =
  $(do info <- reifyRecord 'gadtrec1a
       validateCI info gadtRecVanillaCI)

#if MIN_VERSION_template_haskell(2,6,0)
t43Test :: IO ()
t43Test =
  $(do [decPlain] <- [d| data T43Plain where MkT43Plain :: T43Plain |]
       infoPlain  <- normalizeDec decPlain
       validateDI infoPlain
         DatatypeInfo
           { datatypeName      = mkName "T43Plain"
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkT43Plain"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }

       [decFam] <- [d| data instance T43Fam where  MkT43Fam :: T43Fam |]
       infoFam  <- normalizeDec decFam
       validateDI infoFam
         DatatypeInfo
           { datatypeName      = mkName "T43Fam"
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkT43Fam"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )

t58Test :: IO ()
t58Test =
  $(do [dec] <- [d| data Foo where
                      MkFoo :: a -> Foo |]
       info <- normalizeDec dec
       let a = mkName "a"
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "Foo"
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkFoo"
                   , constructorVars       = [plainTV a]
                   , constructorContext    = []
                   , constructorFields     = [VarT a]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
   )
#endif

#if MIN_VERSION_template_haskell(2,7,0)
dataFamilyTest :: IO ()
dataFamilyTest =
  $(do info <- reifyDatatype 'DFMaybe
       let a = mkName "a"
       validateDI info
         DatatypeInfo
           { datatypeName      = ''DF
           , datatypeContext   = []
           , datatypeVars      = [kindedTV a starK]
           , datatypeInstTypes = [AppT (ConT ''Maybe) (VarT a)]
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'DFMaybe
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Int, ListT `AppT` VarT a]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
  )

ghc78bugTest :: IO ()
ghc78bugTest =
  $(do info <- reifyDatatype 'DF1
       let c    = mkName "c"
           cVar = VarT c
       validateDI info
         DatatypeInfo
           { datatypeName      = ''DF1
           , datatypeContext   = []
           , datatypeVars      = [kindedTV c starK]
           , datatypeInstTypes = [SigT cVar starK]
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'DF1
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [cVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
  )

quotedTest :: IO ()
quotedTest =
  $(do [dec] <- [d| data instance Quoted a = MkQuoted a |]
       info  <- normalizeDec dec
       let a    = mkName "a"
           aVar = VarT a
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "Quoted"
           , datatypeContext   = []
           , datatypeVars      = [plainTV a]
           , datatypeInstTypes = [aVar]
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkQuoted"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [aVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
  )

polyTest :: IO ()
polyTest =
  $(do info <- reifyDatatype 'MkPoly
       let [a,k] = map mkName ["a","k"]
           kVar  = varKCompat k
       validateDI info
         DatatypeInfo
           { datatypeName      = ''Poly
           , datatypeContext   = []
           , datatypeVars      = [
#if __GLASGOW_HASKELL__ >= 800
                                 kindedTV k starK,
#endif
                                 kindedTV a kVar ]
           , datatypeInstTypes = [SigT (VarT a) kVar]
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkPoly
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
  )

gadtFamTest :: IO ()
gadtFamTest =
  $(do info <- reifyDatatype 'MkGadtFam1
       let names@[c,d,e,q]       = map mkName ["c","d","e","q"]
           [cTvb,dTvb,eTvb,qTvb] = map (\v -> kindedTV v starK) names
           [cTy,dTy,eTy,qTy]     = map VarT names
           [cSig,dSig]           = map (\v -> SigT v starK) [cTy,dTy]
       validateDI info
         DatatypeInfo
           { datatypeName      = ''GadtFam
           , datatypeContext   = []
           , datatypeVars      = [cTvb,dTvb]
           , datatypeInstTypes = [cSig,dSig]
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkGadtFam1
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [dTy,cTy]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               , ConstructorInfo
                   { constructorName       = '(:&&:)
                   , constructorVars       = [kindedTV e starK]
                   , constructorContext    = [equalPred cTy (AppT ListT eTy)]
                   , constructorFields     = [eTy,dTy]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = InfixConstructor }
               , ConstructorInfo
                   { constructorName       = '(:^^:)
                   , constructorVars       = []
                   , constructorContext    = [ equalPred cTy (ConT ''Int)
                                             , equalPred dTy (ConT ''Int)
                                             ]
                   , constructorFields     = [ConT ''Int, ConT ''Int]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               , gadtRecFamCI
               , ConstructorInfo
                   { constructorName       = 'MkGadtFam4
                   , constructorVars       = []
                   , constructorContext    = [ equalPred cTy dTy
                                             , equalPred (ConT ''Int) dTy
                                             ]
                   , constructorFields     = [dTy]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               , ConstructorInfo
                   { constructorName       = 'MkGadtFam5
                   , constructorVars       = [kindedTV q starK]
                   , constructorContext    = [ equalPred cTy (ConT ''Bool)
                                             , equalPred dTy (ConT ''Bool)
                                             , equalPred qTy (ConT ''Char)
                                             ]
                   , constructorFields     = [qTy]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
   )

famLocalDecTest1 :: IO ()
famLocalDecTest1 =
  $(do [dec] <- [d| data instance FamLocalDec1 Int = FamLocalDec1Int { mochi :: Double } |]
       info <- normalizeDec dec
       validateDI info
         DatatypeInfo
           { datatypeName      = ''FamLocalDec1
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = [ConT ''Int]
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "FamLocalDec1Int"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Double]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = RecordConstructor [mkName "mochi"] }]
           }
   )

famLocalDecTest2 :: IO ()
famLocalDecTest2 =
  $(do [dec] <- [d| data instance FamLocalDec2 Int (a, b) a = FamLocalDec2Int { fm0 :: (b, a), fm1 :: Int } |]
       info <- normalizeDec dec
       let names            = map mkName ["a", "b"]
           [aTvb,bTvb]      = map plainTV names
           vars@[aVar,bVar] = map (VarT . mkName) ["a", "b"]
       validateDI info
         DatatypeInfo
           { datatypeName      = ''FamLocalDec2
           , datatypeContext   = []
           , datatypeVars      = [aTvb,bTvb]
           , datatypeInstTypes = [ConT ''Int, TupleT 2 `AppT` aVar `AppT` bVar, aVar]
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "FamLocalDec2Int"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [TupleT 2 `AppT` bVar `AppT` aVar, ConT ''Int]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = RecordConstructor [mkName "fm0", mkName "fm1"] }]
           }
   )

recordFamTest :: IO ()
recordFamTest =
  $(do info <- reifyRecord 'famRec1
       validateCI info gadtRecFamCI)

t46Test :: IO ()
t46Test =
  $(do info <- reifyDatatype 'MkT46
       case info of
         DatatypeInfo { datatypeCons = [ConstructorInfo { constructorContext = ctxt }]} ->
           unless (null ctxt) (fail "regression test for ticket #46 failed")
         _ -> fail "T46 should have exactly one constructor"
       [| return () |])

t73Test :: IO ()
t73Test =
  $(do info <- reifyDatatype 'MkT73
       let b    = mkName "b"
           bTvb = kindedTV b starK
           bVar = VarT b
       validateDI info
         DatatypeInfo
           { datatypeName      = ''T73
           , datatypeContext   = []
           , datatypeVars      = [bTvb]
           , datatypeInstTypes = [ConT ''Int, SigT bVar starK]
           , datatypeVariant   = DataInstance
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT73
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [bVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor }]
           }
   )
#endif

fixityLookupTest :: IO ()
fixityLookupTest =
  $(do Just (Fixity 6 InfixR) <- reifyFixityCompat '(:**:)
       [| return () |])

#if __GLASGOW_HASKELL__ >= 704
resolvePredSynonymsTest :: IO ()
resolvePredSynonymsTest =
  $(do info <- reifyDatatype ''PredSynT
       [cxt1,cxt2,cxt3] <- sequence $ map (mapM resolvePredSynonyms . constructorContext)
                                    $ datatypeCons info
       let mkTest = zipWithM_ (equateCxt "resolvePredSynonymsTest")
           test1 = mkTest cxt1 [classPred ''Show [ConT ''Int]]
           test2 = mkTest cxt2 [classPred ''Show [ConT ''Int]]
           test3 = mkTest cxt3 [equalPred (ConT ''Int) (ConT ''Int)]
       mapM_ (either fail return) [test1,test2,test3]
       [| return () |])
#endif

reifyDatatypeWithConNameTest :: IO ()
reifyDatatypeWithConNameTest =
  $(do info <- reifyDatatype 'Just
       let a = mkName "a"
       validateDI info
         DatatypeInfo
          { datatypeContext   = []
          , datatypeName      = ''Maybe
          , datatypeVars      = [kindedTV a starK]
          , datatypeInstTypes = [SigT (VarT a) starK]
          , datatypeVariant   = Datatype
          , datatypeCons      =
              [ ConstructorInfo
                  { constructorName       = 'Nothing
                  , constructorVars       = []
                  , constructorContext    = []
                  , constructorFields     = []
                  , constructorStrictness = []
                  , constructorVariant    = NormalConstructor
                  }
              , justCI
              ]
          }
   )

reifyConstructorTest :: IO ()
reifyConstructorTest =
  $(do info <- reifyConstructor 'Just
       validateCI info justCI)

#if MIN_VERSION_base(4,7,0)
importedEqualityTest :: IO ()
importedEqualityTest =
  $(do info <- reifyDatatype ''(:~:)
       let names@[a,b] = map mkName ["a","b"]
           [aVar,bVar] = map VarT names
           k           = mkName "k"
           kKind       = varKCompat k
       validateDI info
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''(:~:)
           , datatypeVars      = [
#if __GLASGOW_HASKELL__ >= 800
                                 kindedTV k starK,
#endif
                                 kindedTV a kKind, kindedTV b kKind]
           , datatypeInstTypes = [SigT aVar kKind, SigT bVar kKind]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'Refl
                   , constructorVars       = []
                   , constructorContext    = [equalPred aVar bVar]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )
#endif

#if MIN_VERSION_template_haskell(2,8,0)
kindSubstTest :: IO ()
kindSubstTest =
  $(do k1 <- newName "k1"
       k2 <- newName "k2"
       a  <- newName "a"
       let ty = ForallT [kindedTVSpecified a (VarT k1)] [] (VarT a)
           substTy = applySubstitution (Map.singleton k1 (VarT k2)) ty

           checkFreeVars :: Type -> [Name] -> Q ()
           checkFreeVars t freeVars =
             unless (freeVariables t == freeVars) $
               fail $ "free variables of " ++ show t ++ " should be " ++ show freeVars

       checkFreeVars ty      [k1]
       checkFreeVars substTy [k2]
       [| return () |])

t59Test :: IO ()
t59Test =
  $(do k <- newName "k"
       a <- newName "a"
       let proxyAK  = ConT (mkName "Proxy") `AppT` SigT (VarT a) (VarT k)
                        -- Proxy (a :: k)
           expected = ForallT
#if __GLASGOW_HASKELL__ >= 800
                        [plainTVSpecified k, kindedTVSpecified a (VarT k)]
#else
                        [kindedTVSpecified a (VarT k)]
#endif
                        [] proxyAK
           actual = quantifyType proxyAK
       unless (expected == actual) $
         fail $ "quantifyType does not respect dependency order: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])

t61Test :: IO ()
t61Test =
  $(do let test :: Type -> Type -> Q ()
           test orig expected = do
             actual <- resolveTypeSynonyms orig
             unless (expected == actual) $
               fail $ "Type synonym expansion failed: "
                   ++ unlines [ "Expected: " ++ pprint expected
                              , "Actual:   " ++ pprint actual
                              ]

           idAppT = (ConT ''Id `AppT`)
           a = mkName "a"
       test (SigT (idAppT $ ConT ''Int) (idAppT StarT))
            (SigT (ConT ''Int) StarT)
#if MIN_VERSION_template_haskell(2,10,0)
       test (ForallT [kindedTVSpecified a (idAppT StarT)]
                     [idAppT (ConT ''Show `AppT` VarT a)]
                     (idAppT $ VarT a))
            (ForallT [kindedTVSpecified a StarT]
                     [ConT ''Show `AppT` VarT a]
                     (VarT a))
#endif
#if MIN_VERSION_template_haskell(2,11,0)
       test (InfixT (idAppT $ ConT ''Int) ''Either (idAppT $ ConT ''Int))
            (InfixT (ConT ''Int) ''Either (ConT ''Int))
       test (ParensT (idAppT $ ConT ''Int))
            (ConT ''Int)
#endif
       [| return () |])

t66Test :: IO ()
t66Test =
  $(do [dec] <- [d| data Foo a b :: (* -> *) -> * -> * where
                      MkFoo :: a -> b -> f x -> Foo a b f x |]
       info <- normalizeDec dec
       let [a,b,f,x] = map mkName ["a","b","f","x"]
           fKind     = arrowKCompat starK starK
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "Foo"
           , datatypeContext   = []
           , datatypeVars      = [ plainTV a, plainTV b
                                 , kindedTV f fKind, kindedTV x starK ]
           , datatypeInstTypes = [ VarT a, VarT b
                                 , SigT (VarT f) fKind, SigT (VarT x) starK ]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkFoo"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [VarT a, VarT b, VarT f `AppT` VarT x]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
   )

t80Test :: IO ()
t80Test = do
  let [k,a,b] = map mkName ["k","a","b"]
      -- forall k (a :: k) (b :: k). ()
      t = ForallT [ plainTVSpecified k
                  , kindedTVSpecified a (VarT k)
                  , kindedTVSpecified b (VarT k)
                  ] [] (ConT ''())

      expected, actual :: [Name]
      expected = []
      actual   = freeVariables t

  unless (expected == actual) $
    fail $ "Bug in ForallT substitution: "
        ++ unlines [ "Expected: " ++ pprint expected
                   , "Actual:   " ++ pprint actual
                   ]
  return ()
#endif

#if MIN_VERSION_template_haskell(2,11,0)
t79Test :: IO ()
t79Test =
  $(do let [a,b,c]  = map mkName ["a","b","c"]
           t        = ForallT [kindedTVSpecified a (UInfixT (VarT b) ''(:+:) (VarT c))] []
                              (ConT ''())
           expected = ForallT [kindedTVSpecified a (ConT ''(:+:) `AppT` VarT b `AppT` VarT c)] []
                              (ConT ''())
       actual <- resolveInfixT t
       unless (expected == actual) $
         fail $ "resolveInfixT does not recur into the kinds of "
             ++ "ForallT type variable binders: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])
#endif

#if __GLASGOW_HASKELL__ >= 800
t37Test :: IO ()
t37Test =
  $(do infoA <- reifyDatatype ''T37a
       let names@[k,a] = map mkName ["k","a"]
           [kVar,aVar] = map VarT names
           kSig        = SigT kVar starK
           aSig        = SigT aVar kVar
           kTvb        = kindedTV k starK
           aTvb        = kindedTV a kVar
       validateDI infoA
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''T37a
           , datatypeVars      = [kTvb, aTvb]
           , datatypeInstTypes = [kSig, aSig]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT37a
                   , constructorVars       = []
                   , constructorContext    = [equalPred kVar (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }

       infoB <- reifyDatatype ''T37b
       validateDI infoB
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''T37b
           , datatypeVars      = [kTvb, aTvb]
           , datatypeInstTypes = [aSig]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT37b
                   , constructorVars       = []
                   , constructorContext    = [equalPred kVar (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }

       infoC <- reifyDatatype ''T37c
       validateDI infoC
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''T37c
           , datatypeVars      = [kTvb, aTvb]
           , datatypeInstTypes = [aSig]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT37c
                   , constructorVars       = []
                   , constructorContext    = [equalPred aVar (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )

polyKindedExTyvarTest :: IO ()
polyKindedExTyvarTest =
  $(do info <- reifyDatatype ''T48
       let [a,x] = map mkName ["a","x"]
           aVar  = VarT a
       validateDI info
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''T48
           , datatypeVars      = [kindedTV a starK]
           , datatypeInstTypes = [SigT aVar starK]
           , datatypeVariant   = Datatype
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT48
                   , constructorVars       = [kindedTV x aVar]
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Prox `AppT` VarT x]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
       -- Because validateCI uses a type variable substitution to normalize
       -- away any alpha-renaming differences between constructors, it
       -- unfortunately does not check if the uses of `a` in datatypeVars and
       -- constructorVars are the same. We perform this check explicitly here.
       case info of
         DatatypeInfo { datatypeVars = [v1]
                      , datatypeCons =
                          [ConstructorInfo { constructorVars = [v2] }] }
           |  a1 <- tvName v1, starK == tvKind v1, VarT a2 <- tvKind v2
           -> unless (a1 == a2) $
                fail $ "Two occurrences of the same variable have different names: "
                    ++ show [a1, a2]
       [| return () |]
   )

t75Test :: IO ()
t75Test =
  $(do info <- reifyDatatype ''T75
       case datatypeCons info of
         [c] -> let datatypeVarTypes    = map (VarT . tvName) $ datatypeVars info
                    constructorVarKinds = map tvKind $ constructorVars c in
                unless (datatypeVarTypes == constructorVarKinds) $
                  fail $ "Mismatch between datatypeVars and constructorVars' kinds: "
                      ++ unlines [ "datatypeVars:           "
                                     ++ pprint datatypeVarTypes
                                 , "constructorVars' kinds: "
                                     ++ pprint constructorVarKinds
                                 ]
         cs  -> fail $ "Unexpected number of constructors for T75: "
                    ++ show (length cs)
       [| return () |]
   )
#endif

#if __GLASGOW_HASKELL__ >= 807
resolveTypeSynonymsVKATest :: IO ()
resolveTypeSynonymsVKATest =
  $(do t  <- [t| T37b @Bool True |]
       t' <- resolveTypeSynonyms t
       unless (t == t') $
         fail $ "Type synonym expansion breaks with visible kind application: "
            ++ show [t, t']
       [| return () |])
#endif

regressionTest44 :: IO ()
regressionTest44 =
  $(do intToInt <- [t| Int -> Int |]
       unified  <- unifyTypes [intToInt, intToInt]
       unless (Map.null unified) (fail "regression test for ticket #44 failed")
       [| return () |])

t63Test :: IO ()
t63Test =
  $(do a <- newName "a"
       b <- newName "b"
       t <- newName "T"
       let tauType = ArrowT `AppT` VarT a `AppT` (ArrowT `AppT` VarT b
                       `AppT` (ConT t `AppT` VarT a))
           sigmaType = ForallT [plainTVSpecified b] [] tauType
           expected = ForallT [plainTVSpecified a, plainTVSpecified b] [] tauType
           actual   = quantifyType sigmaType
       unless (expected == actual) $
         fail $ "quantifyType does not collapse consecutive foralls: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])

t70Test :: IO ()
t70Test =
  $(do a <- newName "a"
       b <- newName "b"
       let [aVar, bVar] = map VarT    [a, b]
           [aTvb, bTvb] = map plainTV [a, b]
       let fvsABExpected = [aTvb, bTvb]
           fvsABActual   = freeVariablesWellScoped [aVar, bVar]

           fvsBAExpected = [bTvb, aTvb]
           fvsBAActual   = freeVariablesWellScoped [bVar, aVar]

           check expected actual =
             unless (expected == actual) $
               fail $ "freeVariablesWellScoped does not preserve left-to-right order: "
                   ++ unlines [ "Expected: " ++ pprint expected
                              , "Actual:   " ++ pprint actual
                              ]

       check fvsABExpected fvsABActual
       check fvsBAExpected fvsBAActual

       [| return () |])
