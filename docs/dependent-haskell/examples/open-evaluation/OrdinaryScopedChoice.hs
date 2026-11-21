{-# LANGUAGE Haskell2010 #-}
module OrdinaryScopedChoice (readChoice, readDescription) where

import ScopedClassMethod

-- | Read the value returned by the imported method at the caller's instance.
readChoice :: Choose a => a -> Bool
readChoice x = case chooseTagged x of
    Tagged b -> b

-- | Consume the indexed choice without exposing the index in the result.
readDescription :: (Choose r, Show a) => r -> a -> (Bool, String)
readDescription x payload = case describeChoice x payload of
    (Tagged flag, description) -> (flag, description)
