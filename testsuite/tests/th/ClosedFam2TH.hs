{-# LANGUAGE TemplateHaskell, TypeFamilies, PolyKinds, TypeApplications, TypeFamilyDependencies #-}

module ClosedFam2 where

import Language.Haskell.TH

$( return [ ClosedTypeFamilyD
              (TypeFamilyHead
                (mkName "Equals")
                [ KindedTV (mkName "a") () (VarT (mkName "k"))
                , KindedTV (mkName "b") () (VarT (mkName "k")) ]
                ( TyVarSig (KindedTV (mkName "r") () (VarT (mkName "k"))))
                Nothing)
              [ TySynEqn Nothing
                         (AppT (AppT (ConT (mkName "Equals")) (VarT (mkName "a")))
                               (VarT (mkName "a")))
                         (ConT (mkName "Int"))
              , TySynEqn Nothing
                         (AppT (AppT (ConT (mkName "Equals")) (VarT (mkName "a")))
                               (VarT (mkName "b")))
                         (ConT (mkName "Bool")) ] ])

a :: Equals b b
a = (5 :: Int)

b :: Equals Int Bool
b = False

$( return [ ClosedTypeFamilyD
               (TypeFamilyHead
                (mkName "Foo")
                [ KindedTV (mkName "a") () (VarT (mkName "k"))]
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
