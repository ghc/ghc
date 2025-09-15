{-# LANGUAGE TemplateHaskell, UnboxedTuples #-}

module TH_unboxedSingleton where

f :: () -> (# Int #)
f () = $( [| (# 3 #) |] )

