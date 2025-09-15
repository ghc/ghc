module T22913 where

class FromSourceIO a where
    fromSourceIO :: a
instance FromSourceIO (Maybe o) where
    fromSourceIO = undefined
    {-# SPECIALISE INLINE fromSourceIO :: Maybe o #-}
 -- This SPECIALISE pragma caused a Core Lint error
 -- due to incorrectly scoping the type variable 'o' from the instance header
 -- over the SPECIALISE pragma.
