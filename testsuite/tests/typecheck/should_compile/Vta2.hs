{-# LANGUAGE RankNTypes, TypeApplications #-}


module Vta2 where

checkIf :: Bool -> (forall a. a -> a) -> (Bool, Int)
checkIf _ = if True
            then \f -> (f True, f 5)
            else \f -> (f False, f @Int 3)

checkCase :: Bool -> (forall a. a -> a) -> (Bool, Int)
checkCase _ = case True of
                True -> \f -> (f True, f 5)
                False -> \f -> (f False, f @Int 3)
