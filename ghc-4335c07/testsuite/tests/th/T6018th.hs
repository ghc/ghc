{-# LANGUAGE TypeFamilyDependencies, DataKinds, UndecidableInstances,
             PolyKinds #-}
module T6018th where

import Language.Haskell.TH

-- Test that injectivity works correct with TH. This test is not as exhaustive
-- as the original T6018 test.

-- type family F a b c = (result :: k) | result -> a b c
-- type instance F Int  Char Bool = Bool
-- type instance F Char Bool Int  = Int
-- type instance F Bool Int  Char = Char
$( return
   [ OpenTypeFamilyD (TypeFamilyHead
       (mkName "F")
       [ PlainTV (mkName "a"), PlainTV (mkName "b"), PlainTV (mkName "c") ]
       (TyVarSig (KindedTV (mkName "result") (VarT (mkName "k"))))
       (Just $ InjectivityAnn (mkName "result")
                             [(mkName "a"), (mkName "b"), (mkName "c") ]))
   , TySynInstD
       (mkName "F")
       (TySynEqn [ ConT (mkName "Int"), ConT (mkName "Char")
                 , ConT (mkName "Bool")]
                 ( ConT (mkName "Bool")))
   , TySynInstD
       (mkName "F")
       (TySynEqn [ ConT (mkName "Char"), ConT (mkName "Bool")
                 , ConT (mkName "Int")]
                 ( ConT (mkName "Int")))
   , TySynInstD
       (mkName "F")
       (TySynEqn [ ConT (mkName "Bool"), ConT (mkName "Int")
                 , ConT (mkName "Char")]
                 ( ConT (mkName "Char")))
   ] )

-- this is injective - a type variables mentioned on LHS is not mentioned on RHS
-- but we don't claim injectivity in that argument.
--
-- type family J a (b :: k) = r | r -> a
---type instance J Int b = Char
$( return
   [ OpenTypeFamilyD (TypeFamilyHead
       (mkName "J")
       [ PlainTV (mkName "a"), KindedTV (mkName "b") (VarT (mkName "k")) ]
       (TyVarSig (PlainTV (mkName "r")))
       (Just $ InjectivityAnn (mkName "r") [mkName "a"]))
   , TySynInstD
       (mkName "J")
       (TySynEqn [ ConT (mkName "Int"), VarT (mkName "b") ]
                 ( ConT (mkName "Int")))
   ] )

-- Closed type families

-- type family IClosed (a :: *) (b :: *) (c :: *) = r | r -> a b where
--     IClosed Int  Char Bool = Bool
--     IClosed Int  Char Int  = Bool
--     IClosed Bool Int  Int  = Int

$( return
   [ ClosedTypeFamilyD (TypeFamilyHead
       (mkName "I")
       [ KindedTV (mkName "a") StarT, KindedTV (mkName "b") StarT
       , KindedTV (mkName "c") StarT ]
       (TyVarSig (PlainTV (mkName "r")))
       (Just $ InjectivityAnn (mkName "r") [(mkName "a"), (mkName "b")]))
       [ TySynEqn [ ConT (mkName "Int"), ConT (mkName "Char")
                  , ConT (mkName "Bool")]
                  ( ConT (mkName "Bool"))
       , TySynEqn [ ConT (mkName "Int"), ConT (mkName "Char")
                  , ConT (mkName "Int")]
                  ( ConT (mkName "Bool"))
       , TySynEqn [ ConT (mkName "Bool"), ConT (mkName "Int")
                  , ConT (mkName "Int")]
                  ( ConT (mkName "Int"))
       ]
   ] )

-- reification test
$( do { decl@([ClosedTypeFamilyD (TypeFamilyHead _ _ _ (Just inj)) _]) <-
               [d| type family Bak a = r | r -> a where
                        Bak Int  = Char
                        Bak Char = Int
                        Bak a    = a |]
      ; return decl
      }
 )

-- Check whether incorrect injectivity declarations are caught

-- type family I a b c = r | r -> a b
-- type instance I Int  Char Bool = Bool
-- type instance I Int  Int  Int  = Bool
-- type instance I Bool Int  Int  = Int
$( return
   [ OpenTypeFamilyD (TypeFamilyHead
       (mkName "H")
       [ PlainTV (mkName "a"), PlainTV (mkName "b"), PlainTV (mkName "c") ]
       (TyVarSig (PlainTV (mkName "r")))
       (Just $ InjectivityAnn (mkName "r")
                             [(mkName "a"), (mkName "b") ]))
   , TySynInstD
       (mkName "H")
       (TySynEqn [ ConT (mkName "Int"), ConT (mkName "Char")
                 , ConT (mkName "Bool")]
                 ( ConT (mkName "Bool")))
   , TySynInstD
       (mkName "H")
       (TySynEqn [ ConT (mkName "Int"), ConT (mkName "Int")
                 , ConT (mkName "Int")]
                 ( ConT (mkName "Bool")))
   , TySynInstD
       (mkName "H")
       (TySynEqn [ ConT (mkName "Bool"), ConT (mkName "Int")
                 , ConT (mkName "Int")]
                 ( ConT (mkName "Int")))
   ] )
