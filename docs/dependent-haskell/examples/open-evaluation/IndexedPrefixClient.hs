{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module IndexedPrefixClient (matchIndex) where

import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import IndexedPrefix (negatedCons, withElements)

-- | Match the stored equality with an exceptional head and tail.
matchIndex :: IO Bool
matchIndex = do
    b <- newIORef (error "forced indexed list head" :: Bool) >>= readIORef
    bs <- newIORef (error "forced indexed list tail" :: [Bool]) >>= readIORef
    pure (withElements (negatedCons b bs) (\_ proof -> case proof of Refl -> True))
