{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples    #-}

module Main where

main :: IO ()
main = pure ()

foo :: Bounded a => (# a, Int #)
foo = (# minBound, 0 #)

{-# SPECIALISE foo :: (# Bool, Int #) #-}
