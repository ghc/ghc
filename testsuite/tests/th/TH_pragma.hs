{-# OPTIONS -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module TH_pragma where


$( [d| foo :: Int -> Int
       {-# NOINLINE foo #-}
       foo x = x + 1    |] )

$( [d| bar :: Num a => a -> a
       {-# SPECIALISE INLINE [~1] bar :: Float -> Float #-}
       bar x = x * 10        |] )
