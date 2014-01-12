{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module T8499 where

import Language.Haskell.TH

$( do TyConI (DataD _ _ [PlainTV tvb_a] _ _) <- reify ''Maybe
      my_a <- newName "a"
      return [TySynD (mkName "SMaybe")
                     [KindedTV my_a (AppT (ConT ''Maybe) (VarT tvb_a))]
                     (TupleT 0)] )
