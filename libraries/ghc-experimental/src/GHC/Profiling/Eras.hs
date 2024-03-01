{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Profiling.Eras ( setUserEra
                     , getUserEra
                     , incrementUserEra
                     ) where

import GHC.Internal.Base

-- | Set the heap profiling era, setting the era to 0 will stop closures being
-- counted.
foreign import ccall setUserEra :: Word -> IO ()

-- | Query the profiling era
foreign import ccall getUserEra :: IO Word

-- | Increment the era by a given amount, and return the new era.
foreign import ccall incrementUserEra :: Word -> IO Word
