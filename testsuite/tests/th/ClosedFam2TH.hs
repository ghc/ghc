{-# LANGUAGE TemplateHaskell, TypeFamilies, PolyKinds, TypeApplications, TypeFamilyDependencies #-}

module ClosedFam2 where

import Language.Haskell.TH

$( return [ KiSigD (mkName "Equals")
              (ArrowT `AppT` VarT (mkName "k") `AppT`
              (ArrowT `AppT` VarT (mkName "k") `AppT`
               VarT (mkName "k")))
          , ClosedTypeFamilyD
              (TypeFamilyHead
                (mkName "Equals")
                [ KindedTV (mkName "a") BndrReq (VarT (mkName "k"))
                , KindedTV (mkName "b") BndrReq (VarT (mkName "k")) ]
                ( TyVarSig (KindedTV (mkName "r") () (VarT (mkName "k"))))
                Nothing)
              [ TySynEqn Nothing
                         (AppT (AppT (AppKindT (ConT (mkName "Equals")) StarT) (VarT (mkName "a")))
                               (VarT (mkName "a")))
                         (ConT (mkName "Int"))
              , TySynEqn Nothing
                         (AppT (AppT (AppKindT (ConT (mkName "Equals")) StarT) (VarT (mkName "a")))
                               (VarT (mkName "b")))
                         (ConT (mkName "Bool")) ] ])

a :: Equals b b
a = (5 :: Int)

b :: Equals Int Bool
b = False

$( return [ KiSigD (mkName "Foo") (ArrowT `AppT` VarT (mkName "k") `AppT` StarT)
          , ClosedTypeFamilyD
               (TypeFamilyHead
                (mkName "Foo")
                [ KindedTV (mkName "a") BndrReq (VarT (mkName "k"))]
                (KindSig StarT ) Nothing )
                [ TySynEqn Nothing
                           (AppT (AppKindT (ConT (mkName "Foo")) StarT)
                                 (VarT (mkName "a")))
                           (ConT (mkName "Int"))
                , TySynEqn Nothing
                           (AppT (AppKindT (ConT (mkName "Foo")) (AppT (AppT ArrowT StarT) (StarT)))
                                 (VarT (mkName "a")))
                           (ConT (mkName "Bool")) ] ])
c :: Foo Int
c = 5

d :: Foo Bool
d = 6

e :: Foo Maybe
e = False
