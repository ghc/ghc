{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module T11721_TH where

import Language.Haskell.TH

data T a where
  MkT :: forall b a. b -> T a

$(return [])

main :: IO ()
main = print
  $(do let rightOrder :: [TyVarBndr] -> Bool
           rightOrder [KindedTV b _, KindedTV a _]
             = nameBase b == "b" && nameBase a == "a"
           rightOrder _ = False

       TyConI (DataD _ _ _ _
                     [ForallC con_tvbs1 _ _] _) <- reify ''T
       DataConI _ (ForallT con_tvbs2 _ _) _ <- reify 'MkT

       if rightOrder con_tvbs1 && rightOrder con_tvbs2
          then [| () |]
          else fail "T11721_TH failed")
