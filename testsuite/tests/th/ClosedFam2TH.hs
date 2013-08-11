{-# LANGUAGE TemplateHaskell, TypeFamilies, PolyKinds #-}

module ClosedFam2 where

import Language.Haskell.TH

$( return [ ClosedTypeFamilyD (mkName "Equals")
                              [ KindedTV (mkName "a") (VarT (mkName "k"))
                              , KindedTV (mkName "b") (VarT (mkName "k")) ]
                              Nothing
                              [ TySynEqn [ (VarT (mkName "a"))
                                         , (VarT (mkName "a")) ]
                                         (ConT (mkName "Int"))
                              , TySynEqn [ (VarT (mkName "a"))
                                         , (VarT (mkName "b")) ]
                                         (ConT (mkName "Bool")) ] ])

a :: Equals b b
a = (5 :: Int)

b :: Equals Int Bool
b = False
