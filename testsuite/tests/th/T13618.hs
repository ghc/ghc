{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

data family DF a
data    instance DF [a]       = DFList a
newtype instance DF (Maybe a) = DFMaybe a

$(return [])

main :: IO ()
main = print
  $(do FamilyI (DataFamilyD _ _ _) insts <- reify ''DF
       lift $ all (\case DataInstD _ _ (AppT (ConT _) (AppT _ (VarT v1))) _
                                       [NormalC _ [(_, VarT v2)]] _
                           -> v1 == v2
                         NewtypeInstD _ _ (AppT (ConT _) (AppT _ (VarT v1))) _
                                          (NormalC _ [(_, VarT v2)]) _
                           -> v1 == v2
                         _ -> error "Not a data or newtype instance")
              insts)
