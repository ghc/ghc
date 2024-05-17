{-# LANGUAGE Haskell2010 #-}
{-# LINE 4 "hypsrc-test/src/PositionPragmas.hs" #-}
module PositionPragmas where

{-# LINE 8 "hypsrc-test/src/PositionPragmas.hs" #-}

foo :: String
foo = bar

{-# LINE 23 "hypsrc-test/src/PositionPragmas.hs" #-}

bar :: String
bar = foo 

