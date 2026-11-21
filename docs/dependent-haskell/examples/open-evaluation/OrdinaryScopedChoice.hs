{-# LANGUAGE Haskell2010 #-}
module OrdinaryScopedChoice (readChoice, readDescription, readAtFive, projectFive) where

import Numeric.Natural (Natural)
import RetainedNaturals (readNatural)
import ScopedClassMethod

-- | Read the value returned by the imported method at the caller's instance.
readChoice :: Choose a => a -> Bool
readChoice x = case chooseTagged x of
    Tagged b -> b

-- | Consume the indexed choice without exposing the index in the result.
readDescription :: (Choose r, Show a) => r -> a -> (Bool, String)
readDescription x payload = case describeChoice x payload of
    (Tagged flag, description) -> (flag, description)


-- | Pass an ordinary numeric literal beside the method receiver.
readAtFive :: Choose a => a -> (Bool, Natural)
readAtFive x = case tagAt x 5 of
    (Tagged chosen, reading) -> (chosen, readNatural reading)

-- | Read the length through ordinary pair projection.
projectFive :: Choose a => a -> Natural
projectFive x = readNatural (snd (id (tagAt x 5)))
