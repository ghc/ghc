{-# LANGUAGE TypeFamilyDependencies, DataKinds, UndecidableInstances,
             PolyKinds #-}
module T6018th where

import Language.Haskell.TH
import qualified Data.Kind as K

-- Test that injectivity works correct with TH. This test is not as exhaustive
-- as the original T6018 test.

-- type family F a b c = (result :: k) | result -> a b c
-- type instance F Int  Char Bool = Bool
-- type instance F Char Bool Int  = Int
-- type instance F Bool Int  Char = Char
$( return
   [ OpenTypeFamilyD (TypeFamilyHead
       (mkName "F")
       [ PlainTV (mkName "a") BndrReq, PlainTV (mkName "b") BndrReq, PlainTV (mkName "c") BndrReq ]
       (TyVarSig (KindedTV (mkName "result") () (VarT (mkName "k"))))
       (Just $ InjectivityAnn (mkName "result")
                             [(mkName "a"), (mkName "b"), (mkName "c") ]))
   , TySynInstD
       (TySynEqn Nothing (AppT (AppT (AppT (AppKindT (ConT (mkName "F"))
                       (ConT (mkName "K.Type"))) (ConT (mkName "Int")))
                       (ConT (mkName "Char"))) (ConT (mkName "Bool")))
                 (ConT (mkName "Bool")))

   , TySynInstD
       (TySynEqn Nothing (AppT (AppT (AppT (AppKindT (ConT (mkName "F"))
                       (ConT (mkName "K.Type"))) (ConT (mkName "Char")))
                       (ConT (mkName "Bool"))) (ConT (mkName "Int")))
                 (ConT (mkName "Int")))
   , TySynInstD
       (TySynEqn Nothing (AppT (AppT (AppT (AppKindT (ConT (mkName "F"))
                       (ConT (mkName "K.Type"))) (ConT (mkName "Bool")))
                       (ConT (mkName "Int"))) (ConT (mkName "Char")))
                 (ConT (mkName "Char")))
   ] )

-- this is injective - a type variable mentioned on LHS is not mentioned on RHS
-- but we don't claim injectivity in that argument.
--
-- type family J a (b :: k) = r | r -> a
---type instance J Int b = Char
$( return
   [ OpenTypeFamilyD (TypeFamilyHead
       (mkName "J")
       [ PlainTV (mkName "a") BndrReq, KindedTV (mkName "b") BndrReq (VarT (mkName "k")) ]
       (TyVarSig (PlainTV (mkName "r") ()))
       (Just $ InjectivityAnn (mkName "r") [mkName "a"]))
   , TySynInstD
       (TySynEqn Nothing (AppT (AppT (ConT (mkName "J")) (ConT (mkName "Int")))
                       (VarT (mkName "b")))
                 (ConT (mkName "Char")))
   ] )

-- Closed type families

-- type family IClosed (a :: *) (b :: *) (c :: *) = r | r -> a b where
--     IClosed Int  Char Bool = Bool
--     IClosed Int  Char Int  = Bool
--     IClosed Bool Int  Int  = Int

$( return
   [ ClosedTypeFamilyD (TypeFamilyHead
       (mkName "I")
       [ KindedTV (mkName "a") BndrReq StarT, KindedTV (mkName "b") BndrReq StarT
       , KindedTV (mkName "c") BndrReq StarT ]
       (TyVarSig (PlainTV (mkName "r") ()))
       (Just $ InjectivityAnn (mkName "r") [(mkName "a"), (mkName "b")]))

       [ TySynEqn Nothing (AppT (AppT (AppT (ConT (mkName "I")) (ConT (mkName "Int")))
                                      (ConT (mkName "Char"))) (ConT (mkName "Bool")))
                          (ConT (mkName "Bool"))

       , TySynEqn Nothing (AppT (AppT (AppT (ConT (mkName "I")) (ConT (mkName "Int")))
                                      (ConT (mkName "Char"))) (ConT (mkName "Int")))
                          (ConT (mkName "Bool"))

       , TySynEqn Nothing (AppT (AppT (AppT (ConT (mkName "I")) (ConT (mkName "Bool")))
                                      (ConT (mkName "Int"))) (ConT (mkName "Int")))
                          (ConT (mkName "Int"))
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
       [ PlainTV (mkName "a") BndrReq, PlainTV (mkName "b") BndrReq, PlainTV (mkName "c") BndrReq ]
       (TyVarSig (PlainTV (mkName "r") ()))
       (Just $ InjectivityAnn (mkName "r")
                             [(mkName "a"), (mkName "b") ]))

   , TySynInstD
         (TySynEqn Nothing (AppT (AppT (AppT (ConT (mkName "H")) (ConT (mkName "Int")))
                                       (ConT (mkName "Char"))) (ConT (mkName "Bool")))
                           (ConT (mkName "Bool")))

   , TySynInstD
         (TySynEqn Nothing (AppT (AppT (AppT (ConT (mkName "H")) (ConT (mkName "Int")))
                                       (ConT (mkName "Int"))) (ConT (mkName "Int")))
                           (ConT (mkName "Bool")))

   , TySynInstD
         (TySynEqn Nothing (AppT (AppT (AppT (ConT (mkName "H")) (ConT (mkName "Bool")))
                                       (ConT (mkName "Int"))) (ConT (mkName "Int")))
                           (ConT (mkName "Int")))
   ] )
