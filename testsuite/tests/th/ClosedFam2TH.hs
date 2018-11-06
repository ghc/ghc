{-# LANGUAGE TemplateHaskell, TypeFamilies, PolyKinds #-}

module ClosedFam2 where

import Language.Haskell.TH

$( return [ ClosedTypeFamilyD
              (TypeFamilyHead
                (mkName "Equals")
                [ KindedTV (mkName "a") (VarT (mkName "k"))
                , KindedTV (mkName "b") (VarT (mkName "k")) ]
                ( TyVarSig (KindedTV (mkName "r") (VarT (mkName "k"))))
                Nothing)
              [ TySynEqn Nothing
                         [ (VarT (mkName "a"))
                         , (VarT (mkName "a")) ]
                         (ConT (mkName "Int"))
              , TySynEqn Nothing
                         [ (VarT (mkName "a"))
                         , (VarT (mkName "b")) ]
                         (ConT (mkName "Bool")) ] ])

a :: Equals b b
a = (5 :: Int)

b :: Equals Int Bool
b = False
