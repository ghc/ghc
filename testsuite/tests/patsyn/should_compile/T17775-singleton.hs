{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Bug where

-- Ryan Scott (on MR !2600) said this failed

type T = forall a. a -> ()

toT :: () -> T
toT x _ = x

pattern ToT :: T -> ()
pattern ToT{x} <- (toT -> x)

-- f (toT -> (x::T)) = True

