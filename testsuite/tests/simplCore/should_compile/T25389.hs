{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-useless-specialisations #-}

module T25389 where

data Example (b :: Bool) where
  Ex1 :: Int -> Example True
  Ex2 :: Example False

expensive :: Int -> Int
expensive = (*2)

{-# SPECIALISE INLINE op :: Example False -> Int #-}
op :: Example b -> Int
op e = case e of
  Ex1 i -> expensive i
  Ex2 -> 0
