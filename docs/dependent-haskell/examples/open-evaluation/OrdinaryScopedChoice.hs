{-# LANGUAGE Haskell2010 #-}
module OrdinaryScopedChoice (readChoice) where

import ScopedClassMethod

-- | Read the value returned by the imported method at the caller's instance.
readChoice :: Choose a => a -> Bool
readChoice x = case chooseTagged x of
    Tagged b -> b
