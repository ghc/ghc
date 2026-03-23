{-# OPTIONS -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
-- regression test for bug introduced (and fixed) while developing !15899
module TH_pragmaSpecOld where

$( [d| foo :: Num a => a -> a
       {-# SPECIALISE foo :: Int -> Int #-}
       {-# SPECIALISE foo :: Double -> Double #-}
       foo x = x + 1    |] )

$( [d| bar :: Num a => a -> a
       {-# SPECIALISE INLINE [1] bar :: Float -> Float #-}
       {-# SPECIALISE INLINE [1] bar :: Double -> Double #-}
       bar x = x * 10   |] )
