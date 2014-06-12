{-# LANGUAGE FlexibleInstances, TemplateHaskell, PolyKinds, TypeFamilies #-}

module T9160 where
import Language.Haskell.TH

$( do { cls_nm <- newName "C"
      ; a_nm   <- newName "a"
      ; k_nm   <- newName "k"
      ; f_nm   <- newName "F"
      ; return [ClassD [] cls_nm [KindedTV a_nm (VarT k_nm)] []
                    [FamilyD TypeFam f_nm [] (Just (VarT k_nm))]] } )

-- Splices in:
--     class C (a :: k) where
--       type F :: k

instance C (a :: *) where
  type F = Maybe   -- Should be illegal

